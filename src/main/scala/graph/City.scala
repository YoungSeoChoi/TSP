package graph

/**
  * City object
  * @param name name of city
  * @param loc location of city
  */
class City(val name: String, val loc: (Double, Double)) {
  // TODO should check Chap.30 Object Equality and revise this method
  override def equals(obj: Any): Boolean = obj match {
    case that: City => this.name == that.name && this.loc == that.loc
    case _ => false
  }

  override def toString: String = name
}

// XXX Is edge object necessary?
