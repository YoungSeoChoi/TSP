package util

import graph.{City, Edges}

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
    * @param cities
    * @return edges
    */
  def getEdge(cities: List[City]): Edges = {
    val pairs: List[List[City]] = cities.combinations(2).toList
    new Edges(pairs.map(x => (Set(x(0), x(1)), distance(x(0).loc, x(1).loc))).toMap)
  }

  /**
    * Traverse given travel course
    * @param edges
    * @param citySeq travel course
    * @return
    */
  def traverse(edges: Edges, citySeq: List[City]): Double = {
    val seq2 = citySeq.tail ::: List(citySeq.head)
    val pairs: List[(City, City)] = citySeq.zip(seq2)
    val road: List[Double] = for (
      p <- pairs;
      edge = edges(p._1, p._2)
    ) yield edge
    road.sum
  }
}
