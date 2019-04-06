package graph

class Edges(edges: Map[Set[City], Double]) {
  def apply(city1: City, city2: City): Double = {
    if (city1.equals(city2)) 0
    else edges(Set(city1, city2))
  }
}
