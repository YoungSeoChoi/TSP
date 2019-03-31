package util

import scala.io.Source

object FileProcessor {
  /**
    * Read the file that has information of city location
    * @param fileName
    * @return
    */
  def fileReader(fileName: String): Map[String, (Double, Double)] = {
    // No ANSI encoding!
    val lines: List[String] = Source.fromFile(fileName)("Unicode").getLines.toList
    lines.map(x => {val a = x.split(' '); (a(0).toString, (a(1).toDouble, a(2).toDouble))}).toMap
  }
}
