package algorithm

import graph.{City, Edges}

/**
  * Abstract structure of TSP solver
  */
trait Solver {
  /**
    * Solve the TSP with respective algorithm
    * @param cities list of city
    * @param edges given map between two cities and distance of those
    * @return total distance that traveler move
    */
  def solve(cities: List[City], edges: Edges): (List[City], Double)
}
