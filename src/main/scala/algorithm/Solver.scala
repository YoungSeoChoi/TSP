package algorithm

import graph.{City, Edges}

/**
  * Abstract structure of TSP solver
  */
abstract class Solver(cities: List[City], edges: Edges) {
  /**
    * Solve the TSP with respective algorithm
    * @param cities list of city
    * @param edges given map between two cities and distance of those
    * @return total distance that traveler move
    */
  def solve: (List[City], Double)
}
