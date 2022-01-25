package test

import math._
import scala.util._
import scala.io.StdIn._

/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 * */
object Solution extends App {
  val Array(mmm, aaa, nnn) = (readLine split " ").filter(_ != "").map(_.toInt)
  var sum = 0
  (1 to nnn).foreach(x => sum = sum + (x * mmm))
  val to_return = sum - aaa
  println(Math.max(to_return, 0).toString)
}