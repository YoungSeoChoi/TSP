package util

import scala.compat.Platform

object Timer {
  def getTime(p: => Unit): Unit = {
    val start = Platform.currentTime
    p
    val end = Platform.currentTime
    println("time: " + ((end - start) / 1000.0).toString)
  }
}
