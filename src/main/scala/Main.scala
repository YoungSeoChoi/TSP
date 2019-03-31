import algorithm.bruteforce.BruteSolver
import graph.City
import util.{FileProcessor, Util}

object Main {
  def main(args: Array[String]): Unit = {
    // input city with location of city using x, y coordination
    val cities: List[City] = FileProcessor.fileReader("city.txt")
    val cityEdges = Util.getEdge(cities)

    val salesMan = new BruteSolver
    println(salesMan.solve(cities, cityEdges))
  }
}
