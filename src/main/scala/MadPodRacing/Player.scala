package MadPodRacing

import scala.io.StdIn._

object Player extends App {

  while (true) {
    val Array(x, y, nextCheckpointX, nextCheckpointY, nextCheckpointDist, nextCheckpointAngle) = (readLine split " ").filter(_ != "").map(_.toInt)
    val Array(opponentX, opponentY) = (readLine split " ").filter(_ != "").map(_.toInt)
    val thrust = if (nextCheckpointAngle > 90 || nextCheckpointAngle < -90)
      15 else 100
    println(s"$nextCheckpointX $nextCheckpointY $thrust")
  }
}