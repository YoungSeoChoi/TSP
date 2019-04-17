package algorithm.dynamicprogramming

import algorithm.Solver
import graph.{City, Edges}

import scala.annotation.tailrec


class DPSolver(cities: List[City], edges: Edges) extends Solver(cities, edges) {
  /**
    * solve the TSP using Dynamic Programming
    * @param cities list of city
    * @param edges given map between two cities and distance of those
    * @return total distance that traveler move
    */
  override def solve(): (List[City], Double) = {
//    topDown(cities.head, cities.tail.toSet, edges)
    bottomUp(cities.head, cities.tail.toSet, edges)
  }

  /**
    * Solve the tsp using TopDown algorithm. Because result of function is always same at the same parameter,
    * we can use memoization technique for caching purpose.
    * F(s, tV, d) -> s is start city, tV is set of city that have to visit, d is destination city
    * F(s, {}, d) = edge(s to d)
    * F(s, tV, d) = min(F(s, tV - {x}, x) + edge(x to d))  -> use this recurrence relation
    * @param start start city
    * @param toVisit set of city that will be visited
    * @param edges set of edge
    * @return sequence of city, length of travel
    */
  private def topDown(start: City, toVisit: Set[City], edges: Edges): (List[City], Double) = {
    val mem: collection.mutable.Map[(Set[City], City), (List[City], Double)] = collection.mutable.Map()

    // TODO tailrec
    def loop(start: City, toVisit: Set[City], dest: City): (List[City], Double) = {
      if (toVisit.isEmpty) {
        val res = (dest :: Nil, edges(start, dest))
        mem((Set(), dest)) = res
        res
      }
      else {
        mem.get(toVisit, dest) match {
          case Some(p) => p
          case None =>
            val res: (List[City], Double) = toVisit.map(x => {
              val (l, d) = loop(start, toVisit - x, x)
              (dest :: l, d + edges(x, dest))
            }).minBy(_._2)
            mem += ((toVisit, dest) -> res)
            res
        }
      }
    }
    loop(start, toVisit, start)
  }

  /**
    * Solve the TSP with BottomUp DP algorithm. Build the travel map from bottom.
    * Pass the current map to next recursive function.
    * @param start start city
    * @param toVisit set of city that will be visited
    * @param edges set of edge
    * @return sequence of city, length of travel
    */
  private def bottomUp(start: City, toVisit: Set[City], edges: Edges): (List[City], Double) = {
    /**
      * For each step, heighten all branches in current level map.
      * Pick shortest branch and put in the next map(parameter of rec function)
      * @param n size of left cities
      * @param acc current map
      * @return sequence of city, length of travel
      */
    @tailrec
    def loop(n: Int, acc: Map[(Set[City], City), (List[City], Double)]): (List[City], Double) = {
      if (n == 0) {
        // Go back to start city
        val acc1: Stream[(List[City], Double)] = acc.toStream.map(x => {
          val elem: (List[City], Double) = x._2
          (start :: elem._1, edges(start, elem._1.head) + elem._2)
        })

        val acc2: Stream[(List[City], Double)] = acc1.sortWith((x, y) => x._2 > y._2)

        val acc3: Map[(Set[City], City), (List[City], Double)] = acc2.map(x => (x._1.tail.toSet, x._1.head) -> x).toMap

        acc3((toVisit, start))
      }
      else {
        // TODO Is performance of this algorithm same as imperative style?
        // Use Stream for performance purpose
        // Move one step
        val acc1: Stream[(List[City], Double)] = acc.toStream.flatMap(x => {
          val left: Set[City] = (toVisit -- x._1._1) - x._1._2
          val elem: (List[City], Double) = x._2
          left.map(x => (x :: elem._1, edges(x, elem._1.head) + elem._2))
        })

        // Find the smallest travel path
        val acc2: Stream[(List[City], Double)] = acc1.sortWith((x, y) => x._2 > y._2)

        // Because Map automatically override element with same key, the smallest travel path left in the map
        val acc3: Map[(Set[City], City), (List[City], Double)] = acc2.map(x => (x._1.tail.toSet, x._1.head) -> x).toMap
        loop(n-1, acc3)
      }
    }

    val acc0: Set[((Set[City], City), (List[City], Double))] = for {
      i <- toVisit
      edge = edges(start, i)
    } yield ((Set(i), i), (i :: Nil, edge))

    loop(toVisit.size - 1, acc0.toMap)
  }

}
