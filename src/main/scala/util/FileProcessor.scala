package util

import graph.City

import scala.io.Source

object FileProcessor {
  /**
    * Read the file that has information of city location
    * @param fileName input filename
    * @return list of city
    */
  def fileReader(fileName: String): List[City] = {
    // No ANSI encoding!
    val lines: List[String] = Source.fromFile(fileName)("Unicode").getLines.toList
    lines.map(x => {val a = x.split(' '); new City(a(0).toString, (a(1).toDouble, a(2).toDouble))})
  }

  /**
    * Read the file that has information of optimal solution city sequence
    * @param fileName input filename
    * @return list of city name
    */
  def optReader(fileName: String): List[String] = {
    // No ANSI encoding!
    Source.fromFile(fileName)("Unicode").getLines.toList
  }
}
