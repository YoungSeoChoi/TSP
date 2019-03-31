package algorithm.bruteforce

import algorithm.Solver
import util.Util

class BruteSolver extends Solver {
  /**
    * Using brute force algorithm
    * @param cityLocs locations of cities
    * @param edges    given map between two cities and distance of those
    * @return total distance that traveler move
    */
  override def solve(cityLocs: Map[String, (Double, Double)], edges: Map[Set[String], Double]): (List[String], Double) = {
    val cityNames: List[String] = cityLocs.keys.toList
    val cityPm: List[List[String]] = cityNames.permutations.toList
    cityPm.map(x => (x, Util.traverse(edges, x))).minBy(_._2)
  }
}
