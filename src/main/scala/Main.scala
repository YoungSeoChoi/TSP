import algorithm.bruteforce.BruteSolver
import util.{FileProcessor, Util}

object Main {
  def main(args: Array[String]): Unit = {
    // input city with location of city using x, y coordination
    val cityLocs: Map[String, (Double, Double)] = FileProcessor.fileReader("city.txt")
    val cityEdges = Util.getEdge(cityLocs)

    val salesMan = new BruteSolver
    println(salesMan.solve(cityLocs, cityEdges))
  }
}
