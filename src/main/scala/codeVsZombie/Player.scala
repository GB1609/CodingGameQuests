package main.scala.codeVsZombie

import scala.collection.mutable
import scala.io.StdIn._

case class Human(id: Int, x: Int, y: Int)

case class Zombie(id: Int, x: Int, y: Int, zombieXNext: Int, zombieYNext: Int)

/**
 * Save humans, destroy zombies!
 * */
object Player extends App {
  val humans = mutable.ArrayBuffer.empty[Human]
  val zombies = mutable.ArrayBuffer.empty[Zombie]
  // game loop
  while (true) {
    val Array(x, y) = (readLine split " ").filter(_ != "").map(_.toInt)
    val humanCount = readLine.toInt
    (1 to humanCount).foreach(_ => {
      val Array(humanId, humanX, humanY) = (readLine split " ").map(_.toInt)
      humans += Human(humanId, humanX, humanY)
    })
    val zombieCount = readLine.toInt
    (1 to zombieCount).foreach(ciao => {
      val Array(zombieId, zombieX, zombieY, zombieXNext, zombieYNext) = (readLine split " ").map(_.toInt)
      zombies += Zombie(zombieId, zombieX, zombieY, zombieXNext, zombieYNext)
    })

    Console.err.println("zombies.toString()")
    Console.err.println(zombies.map(_.toString).mkString("\n"))
    Console.err.println("humans.toString()")
    Console.err.println(humans.map(_.toString).mkString("\n"))

    println("0 0") // Your destination coordinates
  }
}