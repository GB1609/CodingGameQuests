package test

import scala.io.StdIn._

object SumDiagonal extends App {
  val n = readLine.toInt
  for (i <- 0 until n) {
    val el = (n - i) - 1
    val numbers = readLine.split(" ").map(_.toInt)
    val t=numbers.map(x=>f"${numbers(el)+x}($x+${numbers(el)})")
    println(t.mkString(" "))
  }
}