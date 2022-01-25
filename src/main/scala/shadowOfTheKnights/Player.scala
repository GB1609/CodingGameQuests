package shadowOfTheKnights

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

  def update(position: (Int, Int), direction: Direction.Direction): Unit = {
    minBatmanX = if (position._2 > minBatmanX && Direction.down(direction)) position._2 else minBatmanX
    minBatmanY = if (position._1 > minBatmanY && Direction.right(direction)) position._1 else minBatmanY
    maxBatmanX = if (position._2 < maxBatmanX && Direction.up(direction)) position._2 else maxBatmanX
    maxBatmanY = if (position._1 < maxBatmanY && Direction.left(direction)) position._1 else maxBatmanY
  }

  def print(): Unit = {
    Console.err.println("%%%%%%%%%%%%%%%%%%%%")
    Console.err.println(f"MIN BATMAN Y $minBatmanY")
    Console.err.println(f"MAX BATMAN Y $maxBatmanY")
    Console.err.println(f"MIN BATMAN X $minBatmanX")
    Console.err.println(f"MAX BATMAN X $maxBatmanX")
  }
}


object Player extends App {

  def check(value: Int, min: Int, max: Int): Int = {
    Console.err.println(s"VALUE: $value min $min max $max")
    if (value < min) min else if (value > max) max else value
  }

  def findBestPosition(direction: Direction.Direction, batmanPosition: (Int, Int), maxJumps: Int)
                      (implicit remember: Remember): (Int, Int) = {

    val currentYBatman = batmanPosition._1
    val currentXBatman = batmanPosition._2

    val stepsUp = Math.max(Math.abs(remember.minBatmanX - currentXBatman) / 2, 1)
    val stepsRight = Math.max(Math.abs(remember.maxBatmanY - currentYBatman) / 2, 1)
    val stepsLeft = Math.max(Math.abs(remember.minBatmanY - currentYBatman) / 2, 1)
    val stepsDown = Math.max(Math.abs(remember.maxBatmanX - currentXBatman) / 2, 1)

    direction match {
      case Direction.U =>
        val newX = check(currentXBatman - stepsUp, remember.minBatmanX, remember.maxBatmanX)
        (currentYBatman, newX)
      case Direction.UR =>
        val newX = check(currentXBatman - stepsUp, remember.minBatmanX, remember.maxBatmanX)
        val newY = check(currentYBatman + stepsRight, remember.minBatmanY, remember.maxBatmanY)
        (newY, newX)
      case Direction.R =>
        val newY = check(currentYBatman + stepsRight, remember.minBatmanY, remember.maxBatmanY)
        (newY, currentXBatman)
      case Direction.DR =>
        val newX = check(currentXBatman + stepsDown, remember.minBatmanX, remember.maxBatmanX)
        val newY = check(currentYBatman + stepsRight, remember.minBatmanY, remember.maxBatmanY)
        (newY, newX)
      case Direction.D =>
        val newX = check(currentXBatman + stepsDown, remember.minBatmanX, remember.maxBatmanX)
        (currentYBatman, newX)
      case Direction.DL =>
        val newX = check(currentXBatman + stepsDown, remember.minBatmanX, remember.maxBatmanX)
        val newY = check(currentYBatman - stepsLeft, remember.minBatmanY, remember.maxBatmanY)
        (newY, newX)
      case Direction.L =>
        val newY = check(currentYBatman - stepsLeft, remember.minBatmanY, remember.maxBatmanY)
        (newY, currentXBatman)
      case Direction.UL =>
        val newX = check(currentXBatman - stepsUp, remember.minBatmanX, remember.maxBatmanX)
        val newY = check(currentYBatman - stepsLeft, remember.minBatmanY, remember.maxBatmanY)
        (newY, newX)
    }

  }


  val inputDims: Array[Int] = readLine().split(" ").map(_.toInt)
  implicit val matrixDims: (Int, Int) = (inputDims.head, inputDims.last)
  var maxJumps: Int = readLine.toInt
  val inputBatmanPosition: Array[Int] = readLine().split(" ").map(_.toInt)
  var batmanPosition: (Int, Int) = (inputBatmanPosition.head, inputBatmanPosition.last)
  implicit val remember: Remember = Remember(0, 0, matrixDims._2 - 1, matrixDims._1 - 1)

  // game loop
  while (true) {
    val bombDirectionInput = readLine
    val directionValue = Direction.values.find(_.toString.equals(bombDirectionInput))

    val newPosition = findBestPosition(directionValue.get, batmanPosition, maxJumps)
    remember.update(batmanPosition, directionValue.get)
    maxJumps = maxJumps - 1
    batmanPosition = newPosition

    println(f"${newPosition._1} ${newPosition._2}")
  }
}
