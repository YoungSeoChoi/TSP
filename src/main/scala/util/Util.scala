package util

import math._

object Util {
  /**
    * Get the distance between two point
    * @param a
    * @param b
    * @return distance
    */
  def distance(a: (Double, Double), b: (Double, Double)): Double = {
    val x: Double = pow(abs(a._1 - b._1), 2)
    val y: Double = pow(abs(a._2 - b._2), 2)
    sqrt(x + y)
  }

  /**
    * Get all edges of cities
    * @param cityLocs location of cites
    * @return edges
    */
  def getEdge(cityLocs: Map[String, (Double, Double)]): Map[Set[String], Double] = {
    val pairs: List[List[String]] = cityLocs.keys.toList.combinations(2).toList
    pairs.map(x => (Set(x(0), x(1)), distance(cityLocs(x(0)), cityLocs(x(1))))).toMap
  }

  /**
    * Traverse given travel course
    * @param edges
    * @param citySeq travel course
    * @return
    */
  def traverse(edges: Map[Set[String], Double], citySeq: List[String]): Double = {
    val seq2 = citySeq.tail ::: List(citySeq.head)
    val pairs: List[Set[String]] = citySeq.zip(seq2).map(x => Set(x._1, x._2))
    val road: List[Double] = for (
      p <- pairs;
      edge = edges(p)
    ) yield edge
    road.sum
  }
}
