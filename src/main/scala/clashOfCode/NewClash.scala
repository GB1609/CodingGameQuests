package clashOfCode

import scala.io.StdIn._

object Solution extends App {
  val s = readLine

  var first=0
  val toFind= s(0)
  s.drop(1).zipWithIndex.foreach(x=>if(first==0 && x._1==toFind) first=x._2)
  // Write an answer using println
  // To debug: Console.err.println("Debug messages...")

  println(s.substring(0,first+1))
}