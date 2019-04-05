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

  // Actually, Because we use top down algorithm, we do not have to calculate all edges between cities
  private def topDown(start: City, toVisit: Set[City], dest: City, edges: Edges): (List[City], Double) = {
    val mem: collection.mutable.Map[(Set[City], City), (List[City], Double)] = collection.mutable.Map()
    for ( i <- toVisit - start ) {
      val edge = edges(start, i)
      mem += ((Set(): Set[City], i) -> (i :: Nil, edge))
    }

    def loop(start: City, toVisit: Set[City], dest: City): (List[City], Double) = {
      if (toVisit.isEmpty) mem((Set(), dest))
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
