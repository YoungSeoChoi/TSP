package util

import graph.{City, Edges}

import math._

object Util {
  /**
    * Get the distance between two point
    * @param a coordinates of city1
    * @param b coordinates of city2
    * @return distance between two cities
    */
  def distance(a: (Double, Double), b: (Double, Double)): Double = {
    val x: Double = pow(abs(a._1 - b._1), 2)
    val y: Double = pow(abs(a._2 - b._2), 2)
    sqrt(x + y)
  }

  /**
    * Get all edges of cities
    * @param cities list of cities
    * @return edges set of edges
    */
  def getEdge(cities: List[City]): Edges = {
    val pairs: List[List[City]] = cities.combinations(2).toList
    new Edges(pairs.map(x => (Set(x.head, x(1)), distance(x.head.loc, x(1).loc))).toMap)
  }

  /**
    * Traverse given travel course
    * @param edges set of edges
    * @param citySeq travel course
    * @return length of travel course
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
