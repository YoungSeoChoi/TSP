package graph

class Edges(edges: Map[Set[City], Double]) {
  edges.foreach(println)
  def apply(city1: City, city2: City): Double = {
    edges(Set(city1, city2))
  }
}
