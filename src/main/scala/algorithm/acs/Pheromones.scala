package algorithm.acs

import graph.City

case class Pheromones(pheromones: Map[Set[City], Double]) {
  def apply(city1: City, city2: City): Double = {
    if (city1.equals(city2)) 0
    else pheromones(Set(city1, city2))
  }
}
