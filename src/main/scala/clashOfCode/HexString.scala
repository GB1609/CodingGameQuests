package clashOfCode

import scala.io.StdIn._
object HexString extends App {
  val n = readLine.toInt
  println(n.toHexString.toUpperCase().replaceAll("0","O"))
}