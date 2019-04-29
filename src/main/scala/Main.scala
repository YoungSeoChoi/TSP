import algorithm.acs.ACSSolver
import graph.City
import util.Timer._
import util.{FileProcessor, Util}

object Main {
  def main(args: Array[String]): Unit = {
    // input city with location of city using x, y coordination
    val cities: List[City] = FileProcessor.fileReader("att48.txt")
    val optTour: List[String] = FileProcessor.optReader("att48.opt.txt")
//    val cities: List[City] = FileProcessor.fileReader("pr2392.txt")
//    val optTour: List[String] = FileProcessor.optReader("pr2392.opt.txt")
    val cityEdges = Util.getEdge(cities)

    val optLength = Util.traverse(cityEdges, cities, optTour)
    println("Optimal Solution: " + optLength)

    val salesManACS = new ACSSolver(cities, cityEdges)
    getTime(println(salesManACS.solve))
  }
}
