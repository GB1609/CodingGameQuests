package shadowOfTheKnights

// https://www.codingame.com/ide/puzzle/shadows-of-the-knight-episode-1
// To debug: Console.err.println("Debug messages...")

import scala.io.StdIn.readLine

object Direction extends Enumeration {
  type Direction = Value
  val U, UR, R, DR, D, DL, L, UL = Value

  def up(direction: Direction.Direction): Boolean = {
    direction.equals(U) || direction.equals(UL) || direction.equals(UR)
  }

  def down(direction: Direction.Direction): Boolean = {
    direction.equals(D) || direction.equals(DL) || direction.equals(DR)
  }

  def left(direction: Direction.Direction): Boolean = {
    direction.equals(L) || direction.equals(DL) || direction.equals(UL)
  }

  def right(direction: Direction.Direction): Boolean = {
    direction.equals(R) || direction.equals(DR) || direction.equals(UR)
  }
}

case class Remember(var minBatmanX: Int, var minBatmanY: Int, var maxBatmanX: Int, var maxBatmanY: Int) {

  def updateMinX(position: (Int, Int)): Unit = minBatmanX = position._2 + 1

  def updateMinY(position: (Int, Int)): Unit = minBatmanY = position._1 + 1

  def updateMaxX(position: (Int, Int)): Unit = maxBatmanX = position._2 - 1

  def updateMaxY(position: (Int, Int)): Unit = maxBatmanY = position._1 - 1
}


object Player extends App {

  def check(value: Int, min: Int, max: Int): Int = {
    if (value < min) min else if (value > max) max else value
  }

  def findBestPosition(direction: Direction.Direction, batmanPosition: (Int, Int))
                      (implicit remember: Remember): (Int, Int) = {

    if (Direction.up(direction)) remember.updateMaxX(batmanPosition)
    if (Direction.down(direction)) remember.updateMinX(batmanPosition)
    if (Direction.left(direction)) remember.updateMaxY(batmanPosition)
    if (Direction.right(direction)) remember.updateMinY(batmanPosition)
    val newX = remember.minBatmanX + ((remember.maxBatmanX - remember.minBatmanX) / 2)
    val newY = remember.minBatmanY + ((remember.maxBatmanY - remember.minBatmanY) / 2)
    (newY, newX)
  }


  val inputDims: Array[Int] = readLine().split(" ").map(_.toInt)
  implicit val matrixDims: (Int, Int) = (inputDims.head, inputDims.last)
  var leftJumps: Int = readLine.toInt
  val maxJumps: Int = leftJumps
  val inputBatmanPosition: Array[Int] = readLine().split(" ").map(_.toInt)
  var batmanPosition: (Int, Int) = (inputBatmanPosition.head, inputBatmanPosition.last)
  implicit val remember: Remember = Remember(0, 0, matrixDims._2 - 1, matrixDims._1 - 1)

  // game loop
  while (true) {
    val bombDirectionInput = readLine
    val directionValue = Direction.values.find(_.toString.equals(bombDirectionInput))

    val newPosition = findBestPosition(directionValue.get, batmanPosition)
    leftJumps = leftJumps - 1
    batmanPosition = newPosition
    println(f"${newPosition._1} ${newPosition._2}")
  }
}
