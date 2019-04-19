package algorithm.bruteforce

import algorithm.Solver
import graph.{City, Edges}
import util.Util

class BruteSolver(cities: List[City], edges: Edges) extends Solver(cities, edges) {
  /**
    * Using brute force algorithm
    * @return total distance that traveler move
    */
  override def solve: (List[City], Double) = {
    val cityPm: List[List[City]] = cities.permutations.toList
    cityPm.map(x => (x, Util.traverse(edges, x))).minBy(_._2)
  }
}
