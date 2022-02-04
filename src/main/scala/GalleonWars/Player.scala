package GalleonWars

/**
 * INFO ABOUT GAME: https://www.codingame.com/ide/puzzle/galleon-wars
 */

import scala.io.StdIn._

abstract class Entity {
  def id: Int

  def x: Int

  def y: Int

  def entityType: String
}

case class Ship(id: Int, x: Int, y: Int, entityType: String, orientation: Int, speed: Int, rumStocks: Int, property: Int) extends Entity {
  override def toString: String = f"ID:$id X:$x Y:$y ENTITYTYPE:$entityType: orientation:$orientation SPEED:$speed RUMSTOCKS:$rumStocks PROPERTY:$property"
}

case class Barrel(id: Int, x: Int, y: Int, entityType: String, rumQuantity: Int) extends Entity {
  override def toString: String = f"ID:$id X:$x Y:$y ENTITYTYPE:$entityType: RUMQUANTITY:$rumQuantity"
}

case class CannonBall(id: Int, x: Int, y: Int, entityType: String, launchedBy: Int, impactIn: Int) extends Entity {
  override def toString: String = f"ID:$id X:$x Y:$y ENTITYTYPE:$entityType: launchedBy:$launchedBy impactIn:$impactIn"
}

case class Mine(id: Int, x: Int, y: Int, entityType: String) extends Entity {
  override def toString: String = f"ID:$id X:$x Y:$y ENTITYTYPE:$entityType:"
}

object EntityName extends Enumeration {
  type Object = Value
  val SHIP, BARREL, MINE, CANNONBALL = Value

  def isMine(entityType: String): Boolean = entityType.equalsIgnoreCase(MINE.toString)

  def isBarrel(entityType: String): Boolean = entityType.equalsIgnoreCase(BARREL.toString)

  def isShip(entityType: String): Boolean = entityType.equalsIgnoreCase(SHIP.toString)

  def isCannonBall(entityType: String): Boolean = entityType.equalsIgnoreCase(CANNONBALL.toString)

  def myProperty(ship: Ship): Boolean = ship.property == 1

  def generateEntity(entityId: String, entityType: String, x: String, y: String, arg1: String, arg2: String, arg3: String, arg4: String): Entity = {
    if (entityType.equalsIgnoreCase(SHIP.toString))
      Ship(entityId.toInt, x.toInt, y.toInt, entityType, arg1.toInt, arg2.toInt, arg3.toInt, arg4.toInt)
    else if (entityType.equalsIgnoreCase(BARREL.toString))
      Barrel(entityId.toInt, x.toInt, y.toInt, entityType, arg1.toInt)
    else if (entityType.equalsIgnoreCase(MINE.toString))
      Mine(entityId.toInt, x.toInt, y.toInt, entityType)
    else
      CannonBall(entityId.toInt, x.toInt, y.toInt, entityType, arg1.toInt, arg2.toInt)
  }

}

object Commands {
  val MOVE = "MOVE"
  val SLOWER = "SLOWER"
  val WAIT = "WAIT"
  val MINE = "MINE"
  val FIRE = "FIRE"

  def move(x: Int, y: Int): String = f"$MOVE $x $y"

  def fire(x: Int, y: Int): String = f"$FIRE $x $y"
}

object Direction {
  val RIGHT = 0
  val LEFT = 3
  val TOP_LEFT = 2
  val TOP_RIGHT = 1
  val BOTTOM_LEFT = 4
  val BOTTOM_RIGHT = 5
}


object Player extends App {

  def euclidianDistance(x1: Int, y1: Int, x2: Int, y2: Int, rotation: Int): Double = {
    val sum1 = math.pow(x2 - x1, 2)
    val sum2 = math.pow(y2 - y1, 2)
    math.sqrt(sum1 + sum2)
  }

  def calculateDistance(x1: Int, y1: Int, x2: Int, y2: Int, rotation: Int): Double = {
    val sum1 = math.abs(x2 - x1)
    val sum2 = math.abs(y2 - y1)
    math.max(sum1, sum2)
  }

  // game loop
  while (true) {
    val myShipCount = readLine.toInt // the number of remaining ships
    val entityCount = readLine.toInt // the number of entities (e.g. ships, mines or cannonballs)
    var objects: Seq[Entity] = Seq.empty
    (0 until entityCount).foreach(_ => {
      val Array(entityId, entityType, x, y, arg1, arg2, arg3, arg4) = readLine split " "
      objects = objects :+ EntityName.generateEntity(entityId, entityType, x, y, arg1, arg2, arg3, arg4)
    })
    val allShips: Seq[Ship] = objects.filter(s => EntityName.isShip(s.entityType)).map(_.asInstanceOf[Ship])
    val myShips: Seq[Ship] = allShips.filter(EntityName.myProperty)
    val enemyShips: Seq[Ship] = allShips.filterNot(EntityName.myProperty)
    val barrels: Seq[Barrel] = objects.filter(b => EntityName.isBarrel(b.entityType)).map(_.asInstanceOf[Barrel])

    val myDistance = myShips.map(ms => {
      val distancesFromBarrel = barrels.map(b => (b.id, b.x, b.y, calculateDistance(b.x, b.y, ms.x, ms.y, ms.orientation)))
      (ms.id, distancesFromBarrel)
    })
    val enemyDistance = enemyShips.map(es => {
      val distancesFromBarrel = barrels.map(b => (b.id, b.x, b.y, calculateDistance(b.x, b.y, es.x, es.y, es.orientation)))
      (es.id, distancesFromBarrel)
    })
    myShips.foreach(ship => {
      val distancesFromBarrel = myDistance.filter(_._1 == ship.id).head._2
      val candidates = distancesFromBarrel.filter(b => enemyDistance.exists(_._2.exists(i => i._1 == b._1 && i._4 > b._4)))
      if (candidates.isEmpty)
        println(Commands.WAIT)
      //println(Commands.fire(enemyShips.head.x, enemyShips.head.y))
      else {
        val barrel = candidates.minBy(_._4)
        println(Commands.move(barrel._2, barrel._3))
      }
    })
  }
}