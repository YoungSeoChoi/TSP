package algorithm.bruteforce

import algorithm.Solver
import graph.{City, Edges}
import util.Util

class BruteSolver extends Solver {
  /**
    * Using brute force algorithm
    * @param cities cities
    * @param edges    given map between two cities and distance of those
    * @return total distance that traveler move
    */
  override def solve(cities: List[City], edges: Edges): (List[City], Double) = {
    val cityPm: List[List[City]] = cities.permutations.toList
    cityPm.map(x => (x, Util.traverse(edges, x))).minBy(_._2)
  }
}
