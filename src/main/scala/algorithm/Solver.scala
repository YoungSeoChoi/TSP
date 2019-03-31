package algorithm

/**
  * Abstract structure of TSP solver
  */
trait Solver {
  /**
    * Solve the TSP with respective algorithm
    * @param cityLocs locations of cities
    * @param edges given map between two cities and distance of those
    * @return total distance that traveler move
    */
  def solve(cityLocs: Map[String, (Double, Double)], edges: Map[Set[String], Double]): (List[String], Double)
}
