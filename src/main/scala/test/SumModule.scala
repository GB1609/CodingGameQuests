package test

import scala.io.StdIn._

object SumModule extends App {
  val m = readLine.toInt
  val n = readLine.toInt
  var inputs = (readLine split " ").map(_.toInt)
//  var sum = 0
//  inputs.foreach(x => {
//    sum = sum + (x % m)
//  })
//  println(sum)
  println(inputs.map(_%m).sum)
}