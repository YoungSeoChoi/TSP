package algorithm.dynamicprogramming

import algorithm.Solver
import graph.{City, Edges}


class DPSolver extends Solver {
  /**
    * solve the TSP using Dynamic Programming
    * @param cities
    * @param edges    given map between two cities and distance of those
    * @return total distance that traveler move
    */
  override def solve(cities: List[City], edges: Edges): (List[City], Double) = {
    topDown(cities.head, cities.tail.toSet, cities.head, edges)
  }

  /**
    * Solve the tsp using TopDown algorithm. Because result of function is always same at the same parameter,
    * we can use memoization technique for caching purpose.
    * F(s, tV, d) : s is start city, tV is set of city that have to visit, d is destination city
    * F(s, {}, d) = edge(s to d)
    * F(s, tV, d) = min(F(s, tV - {x}, x) + edge(x to d)) 이 점화식을 이용한다!
    * @param start start city
    * @param toVisit set of city that will be visited
    * @param dest destination city
    * @param edges set of edge
    * @return sequence of city, length of travel
    */
  private def topDown(start: City, toVisit: Set[City], dest: City, edges: Edges): (List[City], Double) = {
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
          case None => {
            val res: (List[City], Double) = toVisit.map(x => {
              val (l, d) = loop(start, toVisit - x, x)
              (dest :: l, d + edges(x, dest))
            }).minBy(_._2)
            mem += ((toVisit, dest) -> res)
            res
          }
        }
      }
    }
    loop(start, toVisit, dest)
  }
}
