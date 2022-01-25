package thereIsNoSpoon

//https://www.codingame.com/ide/puzzle/there-is-no-spoon-episode-1
// To debug: Console.err.println("Debug messages...")

import scala.io.StdIn._


object Cell {
  val EMPTY = "."
  val NODE = "0"

  def isNode(cell: String): Boolean = cell.equalsIgnoreCase(NODE)

  def isEmpty(cell: String): Boolean = cell.equalsIgnoreCase(EMPTY)

  def define(cell: Char): String = if (cell.toString.equalsIgnoreCase(EMPTY)) EMPTY else NODE
}

/**
 * Don't let the machines win. You are humanity's last hope...
 * */
object Player extends App {

  var matrix: List[(Int, List[(Int, String)])] = List.empty
  val width = readLine.toInt // the number of cells on the X axis
  val height = readLine.toInt // the number of cells on the Y axis
  for (i <- 0 until height) {

    val line: String = readLine // width characters, each either 0 or .
    val formattedLine = line.map(c => Cell.define(c)).zipWithIndex.map(el => (el._2, el._1)).toList
    matrix = matrix :+ (i, formattedLine)
  }

  (0 until height).foreach(numRow => {
    (0 until width).foreach(numCol => {
      val row = matrix(numRow)
      if (Cell.isNode(row._2.filter(_._1==numCol).head._2)) {
        val nodesOnRight = row._2.filter(el => el._1 > numCol && Cell.isNode(el._2))
        val right = if (nodesOnRight.isEmpty) (-1, -1) else (nodesOnRight.map(_._1).min, numRow)
        val cellOnDown: Seq[(Int, String)] = matrix
          .filter(r => r._1 > numRow)
          .map(l => (l._1, l._2.filter(_._1 == numCol).map(_._2).head))
        val nodesDown: Seq[(Int, String)] = cellOnDown.filter(el => Cell.isNode(el._2))
        val down = if (nodesDown.isEmpty) (-1, -1) else (numCol, nodesDown.map(_._1).min)
        //OUTPUT FOR SINGLE NODE
        println(f"$numCol $numRow ${right._1} ${right._2} ${down._1} ${down._2}")
      }
    })
  })


}