package algorithm.acs

import algorithm.Solver
import graph.{City, Edges}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

class ACSSolver(cities: List[City], edges: Edges) extends Solver(cities, edges) {
  /**
    * Solve the TSP with ant colony optimization algorithm (Ant Colony System)
    * @return total distance that traveler move
    */
  override def solve: (List[City], Double) = {
    val pheromone0: Double = getPheromone0(10)
    val pheromones: Pheromones = getPheromones(pheromone0)
    // From the paper, the best number of ants is 10
    val ants: List[Ant] = initialize(10)

    tour(ants, pheromones, pheromone0, 100)
  }

  /**
    * Get initial pheromone tau0, tau0 = 1 / (m * length of nearest neighbor heuristic).
    * (the information of parameter is from ACS paper)
    * @param m number of ants
    * @return initial amount of pheromone
    */
  def getPheromone0(m: Int): Double = {
    // Nearest neighbor heuristic algorithm
    @tailrec
    def nearest(start: City, now: City, visited: Set[City], acc: Double): Double = {
      if (visited.isEmpty) acc + edges(now, start)
      else {
        val next: City = visited.minBy(x => edges(x, now))
        nearest(start, next, visited - next, acc + edges(now, next))
      }
    }
    val tourNearest: Double = nearest(cities.head, cities.head, cities.tail.toSet, 0)
    1 / (m * tourNearest)
  }

  /**
    * Get the initialized pheromones
    * @param pheromone0 initial amount of pheromone
    * @return pheromones map
    */
  def getPheromones(pheromone0: Double): Pheromones = {
    val pairs: List[List[City]] = cities.combinations(2).toList
    Pheromones(pairs.map(x => (Set(x.head, x(1)), pheromone0)).toMap)
  }

  /**
    * Initialize ants. Each ant starts from random city
    * @param m number of ants
    * @return list of ants
    */
  def initialize(m: Int): List[Ant] = {
    val randCity: List[City] = Random.shuffle(cities).take(m)
    randCity.map(x => Ant(x, List(x), 0, x))
  }

  /**
    * Ant Colony System. Global pheromone update is from ACS paper. 1 iteration = 1 tour all cities
    * @param ants list of ants
    * @param pheromones pheromone information
    * @param pheromone0 initial pheromone (For local pheromone update)
    * @param iterations end condition
    * @return best city sequence and length
    */
  def tour(ants: List[Ant], pheromones: Pheromones, pheromone0: Double, iterations: Int): (List[City], Double) = {
    @tailrec
    def loop(n: Int, pheromones: Pheromones, bestTrace: List[City], bestLength: Double): (List[City], Double) = {
      if (n == 0) (bestTrace, bestLength)
      else {
        // ACS State Transition
        // use outer ants because after the tour is over, all ants will come back start city
        val (fAnts, fPheromones) = step(ants, pheromones, pheromone0)

        // Global update
        val bestAnt: Ant = fAnts.minBy(_.length)
        val (bestT, bestL): (List[City], Double) = if (bestLength > bestAnt.length) (bestAnt.trace, bestAnt.length) else (bestTrace, bestLength)
        val deltaPheromone: Double = 1 / bestL
        val edgeTrace: Set[Set[City]] = (bestT zip (bestT.tail :+ bestT.head)).map(x => Set(x._1, x._2)).toSet
        val pheromoneMap: Map[Set[City], Double] = fPheromones.pheromones.map(x => {
          if (edgeTrace.contains(x._1)) (x._1, 0.9 * x._2 + 0.1 * deltaPheromone)
          else (x._1, 0.9 * x._2)
        })
        val ffPheromones: Pheromones = Pheromones(pheromoneMap)

        loop(n - 1, ffPheromones, bestT, bestL)
      }
    }

    loop(iterations, pheromones, Nil, Float.PositiveInfinity)
  }

  /**
    * Move city to next city with local pheromone update.
    * @param ants list of ants
    * @param pheromones pheromone information
    * @param pheromone0 initial pheromone (For local update)
    * @return result of list of ants after one step moved
    */
  def step(ants: List[Ant], pheromones: Pheromones, pheromone0: Double): (List[Ant], Pheromones) = {
    @tailrec
    def loop(n: Int, ants: List[Ant], pheromones: Pheromones): (List[Ant], Pheromones) = {
      // XXX local update 때문에 일단 빌더를 썼는데 내 생각에는 update 함수만 분리한 다음 마지막에 pheromone에 함수들을 한꺼번에 적용하는 것을 어떨까??
      val pheromoneBuilder: mutable.Map[Set[City], Double] = mutable.Map(pheromones.pheromones.toSeq: _*)

      if (n == 0) {
        val b: List[Ant] = ants.map(x => {
          // Local update
          pheromoneBuilder(Set(x.now, x.start)) = 0.9 * pheromones(x.now, x.start) + 0.1 * pheromone0
          // last destination is start position. start position is not included in trace
          Ant(x.start, x.trace, x.length + edges(x.now, x.start), x.start)
        })
        val q: Pheromones = Pheromones(Map(pheromoneBuilder.toSeq: _*))
        (b, q)
      }
      else {
        val b: List[Ant] = ants.map(a => {
          if (Random.nextDouble <= 0.9) { // exploitation
            val left: Set[City] = cities.toSet -- a.trace.toSet
            val next: City = left.maxBy(getPower(a.now, _, pheromones))
            pheromoneBuilder(Set(a.now, next)) = 0.9 * pheromones(a.now, next) + 0.1 * pheromone0
            Ant(next, next :: a.trace, a.length + edges(a.now, next), a.start)
          } else { // exploration
            val rand: Double = Random.nextDouble()
            val left: Set[City] = cities.toSet -- a.trace.toSet
            val powerSum: Double = left.map(getPower(a.now, _, pheromones)).sum
            val prob: List[(City, Double)] = left.map(x => (x, getPower(a.now, x, pheromones) / powerSum)).toList
            val next: City = stochasticSearch(prob, rand)
            pheromoneBuilder(Set(a.now, next)) = 0.9 * pheromones(a.now, next) + 0.1 * pheromone0
            Ant(next, next :: a.trace, a.length + edges(a.now, next), a.start)
          }
        })
        val q: Pheromones = Pheromones(Map(pheromoneBuilder.toSeq: _*))
        loop(n - 1, b, q)
      }
    }

    loop(cities.length - 1, ants, pheromones)
  }

  /**
    * Get the score of edge. It is proportion to amount of pheromone and reciprocal of length
    * @param start start city
    * @param dest destination city
    * @param pheromones pheromone information
    * @return score of edge
    */
  private def getPower(start: City, dest: City, pheromones: Pheromones): Double = {
    pheromones(start, dest) * math.pow(1 / edges(start, dest), 2)
  }

  /**
    * Choose the next destination city with given probability
    * @param prob probability list
    * @param rand given random number
    * @return next city
    */
  @tailrec
  private def stochasticSearch(prob: List[(City, Double)], rand: Double): City = {
    val c = prob.head
    if (rand < c._2) c._1
    else stochasticSearch(prob.tail, rand - c._2)
  }
}
