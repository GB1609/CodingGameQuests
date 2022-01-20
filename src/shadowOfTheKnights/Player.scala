
import scala.io.StdIn.readLine

object Direction extends Enumeration {
  type Direction = Value
  val U, UR, R, DR, D, DL, L, UL = Value
}

case class Remember(var minBatmanX: Int, var minBatmanY: Int, var maxBatmanX: Int, var maxBatmanY: Int,
                    maxDimensionY: Int, maxDimensionX: Int) {

  def update(position: (Int, Int)): Unit = {
    minBatmanX = if (position._2 < minBatmanX) position._2 else minBatmanX
    minBatmanY = if (position._1 < minBatmanY) position._1 else minBatmanY
    maxBatmanX = if (position._2 > maxBatmanX) position._2 else maxBatmanX
    maxBatmanY = if (position._1 > maxBatmanY) position._1 else maxBatmanY
  }

  def print(): Unit = {
    Console.err.println(f"MIN BATMAN X $minBatmanY")
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
        val newX = check(currentXBatman - 1, remember.minBatmanX, remember.maxBatmanX, remember.maxDimensionX)
        (currentYBatman, newX)
      case Direction.UR =>
        val newX = check(currentXBatman - 1, remember.minBatmanX, remember.maxBatmanX, remember.maxDimensionX)
        val newY = check(currentYBatman + 1, remember.minBatmanY, remember.maxBatmanY, remember.maxDimensionY)
        (newY, newX)
      case Direction.R =>
        val sumY = ((remember.maxDimensionY - currentYBatman) / 2) + 1
        val newY = check(currentYBatman + sumY, remember.minBatmanY, remember.maxBatmanY, remember.maxDimensionY)
        (newY, currentXBatman)
      case Direction.DR =>
        val newX = check(currentXBatman + 1, remember.minBatmanX, remember.maxBatmanX, remember.maxDimensionX)
        val newY = check(currentYBatman + 1, remember.minBatmanY, remember.maxBatmanY, remember.maxDimensionY)
        (newY, newX)
      case Direction.D =>
        val sumX = ((remember.maxDimensionX - currentXBatman) / 2) + 1
        val newX = check(currentXBatman + sumX, remember.minBatmanX, remember.maxBatmanX, remember.maxDimensionX)
        (currentYBatman, newX)
      case Direction.DL =>
        val newX = check(currentXBatman + 1, remember.minBatmanX, remember.maxBatmanX, remember.maxDimensionX)
        val newY = check(currentYBatman - 1, remember.minBatmanY, remember.maxBatmanY, remember.maxDimensionY)
        (newY, newX)
      case Direction.L =>
        val newY = check(currentYBatman - 1, remember.minBatmanY, remember.maxBatmanY, remember.maxDimensionY)
        (newY, currentXBatman)
      case Direction.UL =>
        val newX = check(currentXBatman - 1, remember.minBatmanX, remember.maxBatmanX, remember.maxDimensionX)
        val newY = check(currentYBatman - 1, remember.minBatmanY, remember.maxBatmanY, remember.maxDimensionY)
        (newY, newX)
    }

  }


  val inputDims: Array[Int] = readLine().split(" ").map(_.toInt)
  implicit val matrixDims: (Int, Int) = (inputDims.head, inputDims.last)
  var maxJumps: Int = readLine.toInt
  val inputBatmanPosition: Array[Int] = readLine().split(" ").map(_.toInt)
  var batmanPosition: (Int, Int) = (inputBatmanPosition.head, inputBatmanPosition.last)
  implicit val remember: Remember = Remember(batmanPosition._1, batmanPosition._2, batmanPosition._1, batmanPosition._2,
    matrixDims._1 - 1, matrixDims._2 - 1)

  // game loop
  while (true) {
    val bombDirectionInput = readLine
    val directionValue = Direction.values.find(_.toString.equals(bombDirectionInput))

    val newPosition = findBestPosition(directionValue.get, batmanPosition, maxJumps)
    maxJumps = maxJumps - 1
    batmanPosition = newPosition
    remember.update(batmanPosition)
    println(f"${newPosition._1} ${newPosition._2}")
  }
}
