import algorithm.acs.ACSSolver
import algorithm.bruteforce.BruteSolver
import algorithm.dynamicprogramming.DPSolver
import graph.City
import util.{FileProcessor, Util}

object Main {
  def main(args: Array[String]): Unit = {
    // input city with location of city using x, y coordination
    val cities: List[City] = FileProcessor.fileReader("city.txt")
    val cityEdges = Util.getEdge(cities)

    val salesManB = new BruteSolver(cities, cityEdges)
    val salesManDP = new DPSolver(cities, cityEdges)
    val salesManACS = new ACSSolver(cities, cityEdges)
  }
}
