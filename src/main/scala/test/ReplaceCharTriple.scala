package test

import scala.io.StdIn._

object ReplaceCharTriple extends App {
  (0 until 5).foreach(_ => {
    val line = readLine.split(" ")
    val toChange = line.head.zipWithIndex.map(x => {if (x._1 == '0') line(1)(x._2) else line(2)(x._2)})
    println(toChange.mkString)
  })
}