package test

import scala.io.StdIn._

object SumModule extends App {
  val m = readLine.toInt
  val n = readLine.toInt
  val inputs = (readLine split " ").map(_.toInt)
  println(inputs.map(_%m).sum)
}