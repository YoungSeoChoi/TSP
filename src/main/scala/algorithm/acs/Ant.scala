package algorithm.acs

import graph.City

// We can either save start city or not. It depends on design choice
case class Ant(now: City, trace: List[City], length: Double, start: City)
