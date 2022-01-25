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
    minBatmanX = if (position._2 < minBatmanX && Direction.up(direction)) position._2 else minBatmanX
    minBatmanY = if (position._1 < minBatmanY && Direction.left(direction)) position._1 else minBatmanY
    maxBatmanX = if (position._2 > maxBatmanX && Direction.down(direction)) position._2 else maxBatmanX
    maxBatmanY = if (position._1 > maxBatmanY&& Direction.right(direction)) position._1 else maxBatmanY
  }

  def print(): Unit = {
    Console.err.println(f"MIN BATMAN Y $minBatmanY")
    Console.err.println(f"MAX BATMAN Y $maxBatmanY")
    Console.err.println(f"MIN BATMAN X $minBatmanX")
    Console.err.println(f"MAX BATMAN X $maxBatmanX")
  }
}


object Player extends App {

  def check(value: Int, max: Int, min: Int, maxDimension: Int): Int = {
    if (value < 0) 0 else if (value > maxDimension) maxDimension else value
  }

  def findBestPosition(direction: Direction.Direction, batmanPosition: (Int, Int), maxJumps: Int)
                      (implicit remember: Remember): (Int, Int) = {

    val currentYBatman = batmanPosition._1
    val currentXBatman = batmanPosition._2

    direction match {
      case Direction.U =>
        val newX = check(currentXBatman - 1, remember.minBatmanX, remember.maxBatmanX, remember.maxBatmanX)
        (currentYBatman, newX)
      case Direction.UR =>
        val newX = check(currentXBatman - 1, remember.minBatmanX, remember.maxBatmanX, remember.maxBatmanX)
        val newY = check(currentYBatman + 1, remember.minBatmanY, remember.maxBatmanY, remember.maxBatmanY)
        (newY, newX)
      case Direction.R =>
        val sumY = (Math.abs(remember.maxBatmanY - currentYBatman) / 2) + 1
        val newY = check(currentYBatman + sumY, remember.minBatmanY, remember.maxBatmanY, remember.maxBatmanY)
        (newY, currentXBatman)
      case Direction.DR =>
        val newX = check(currentXBatman + 1, remember.minBatmanX, remember.maxBatmanX, remember.maxBatmanX)
        val newY = check(currentYBatman + 1, remember.minBatmanY, remember.maxBatmanY, remember.maxBatmanY)
        (newY, newX)
      case Direction.D =>
        val sumX = Math.abs((remember.maxBatmanX - currentXBatman) / 2) + 1
        val newX = check(currentXBatman + sumX, remember.minBatmanX, remember.maxBatmanX, remember.maxBatmanX)
        (currentYBatman, newX)
      case Direction.DL =>
        val newX = check(currentXBatman + 1, remember.minBatmanX, remember.maxBatmanX, remember.maxBatmanX)
        val newY = check(currentYBatman - 1, remember.minBatmanY, remember.maxBatmanY, remember.maxBatmanY)
        (newY, newX)
      case Direction.L =>
        val newY = check(currentYBatman - 1, remember.minBatmanY, remember.maxBatmanY, remember.maxBatmanY)
        (newY, currentXBatman)
      case Direction.UL =>
        val newX = check(currentXBatman - 1, remember.minBatmanX, remember.maxBatmanX, remember.maxBatmanX)
        val newY = check(currentYBatman - 1, remember.minBatmanY, remember.maxBatmanY, remember.maxBatmanY)
        (newY, newX)
    }

  }


  val inputDims: Array[Int] = readLine().split(" ").map(_.toInt)
  implicit val matrixDims: (Int, Int) = (inputDims.head, inputDims.last)
  var maxJumps: Int = readLine.toInt
  val inputBatmanPosition: Array[Int] = readLine().split(" ").map(_.toInt)
  var batmanPosition: (Int, Int) = (inputBatmanPosition.head, inputBatmanPosition.last)
  implicit val remember: Remember = Remember(0, 0, matrixDims._2 - 1, matrixDims._1 - 1)
  remember.print()

  // game loop
  while (true) {
    val bombDirectionInput = readLine
    val directionValue = Direction.values.find(_.toString.equals(bombDirectionInput))

    val newPosition = findBestPosition(directionValue.get, batmanPosition, maxJumps)
    maxJumps = maxJumps - 1
    batmanPosition = newPosition
    remember.update(batmanPosition, directionValue.get)
    println(f"${newPosition._1} ${newPosition._2}")
  }
}
