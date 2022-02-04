package clashOfCode

import scala.io.StdIn._

object Subnet extends App {
  val n = 25
  if (n > 32) println("INVALID")
  val ones = "1" * n
  val zero = "0" * (32 - n)
  val output1 = f"$ones$zero".patch(8, ".", 0)
  val output2 = output1.patch(17, ".", 0)
  val output3 = output2.patch(26, ".", 0)
  println(output3)
  val bla = output3.split("\\.").map(_.count(_.toString.equals("1")))
  println(bla.mkString("Array(", ", ", ")"))
  val bla2 = bla.map(x => (math.pow(2, x) - 1).toInt).mkString(".")
  println(bla2)
}