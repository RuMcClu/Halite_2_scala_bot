package hlt

import scala.annotation.tailrec

class Navigation(gameMap: GameMap, var ship: Ship, var target: Position) {

  val maxCorrections = Constants.MAX_CORRECTIONS
  val avoidObstacles = true
  val angularStepRad = Math.PI / 360
  val valAround = ship.orientTowardsInRad(target)
  val maxThrust = Constants.MAX_SPEED
  def navigateTo(): Option[ThrustMove] = {
    navigateTowards(gameMap, target, target, maxThrust, avoidObstacles,  maxCorrections, angularStepRad, valAround)
  }

  @tailrec
  private def navigateTowards(gameMap: GameMap,
                              targetPos: Position,
                              originalTarget: Position,
                              maxThrust: Int,
                              avoidObstacles: Boolean,
                              maxCorrections: Int,
                              angularStepRad: Double,
                              valAround: Double): Option[ThrustMove] = {
    if (maxCorrections <= 0) {
      //DebugLog.addLog("timed out on navigate iterator")
      None
    } else {
      val distance = ship.getDistanceTo(targetPos)
      val angleRad = valAround
      if ((avoidObstacles && !gameMap.objectsBetween(ship, targetPos).isEmpty)||
        gameMap.posIsOutsideMap(targetPos)) {
        val originalRad = ship.orientTowardsInRad(originalTarget)
        val diffRad = originalRad - angleRad
        val newRad = originalRad + diffRad
        val newRadWithStep = diffRad match {
          case x if (x >= 0) => {
            //DebugLog.addLog(x.toString)
            newRad + angularStepRad}
          case _ => newRad - angularStepRad
        }
        //DebugLog.addLog(newRad.toString)
        val newTargetDx = Math.cos(newRadWithStep) * distance
        val newTargetDy = Math.sin(newRadWithStep) * distance
        val newTarget = new Position(ship.getXPos + newTargetDx, ship.getYPos + newTargetDy)
        //DebugLog.addLog("newTarget = " + newTarget.toString)
        navigateTowards(gameMap,
                        newTarget,
                        originalTarget,
                        maxThrust,
                        avoidObstacles = true,
                        maxCorrections - 1,
                        angularStepRad,
                        newRadWithStep)
      } else {
        var thrust = 0
        if (distance < maxThrust && avoidObstacles) { // Do not round up, since overshooting might cause collision.
          thrust = distance.toInt
        } else {
          thrust = maxThrust
        }
        //check if we're going to hit a ship
        //val newX = ship.getXPos + (Math.cos(angleRad) * thrust)
        //val newY = ship.getYPos + (Math.sin(angleRad) * thrust)
        //val newTraj = new Trajectory (ship.getXPos,ship.getYPos,newX,newY)

        /*if (avoidSelf && trajTable.exists(_.intersects(newTraj))){
          if (trajTable.exists(_.collidesWith(newTraj))){
            val newTargetDx = Math.cos(angleRad + angularStepRad) * distance
            val newTargetDy = Math.sin(angleRad + angularStepRad) * distance
            val newTarget = new Position(ship.getXPos + newTargetDx, ship.getYPos + newTargetDy)
            navigateTowards(gameMap,
                            trajTable,
                            newTarget,
                            maxThrust,
                            avoidObstacles = true,
                            avoidSelf = true,
                            maxCorrections - 1,
                            angularStepRad,
                            valAround)

          } else {
          val angleDeg = Util.angleRadToDegClipped(angleRad)
          trajTable.addTraj(newTraj)
          Some(new ThrustMove(ship, angleDeg, thrust))
          }

        } else {*/
          val angleDeg = Util.angleRadToDegClipped(angleRad)
          //trajTable.addTraj(newTraj)
          Some(new ThrustMove(ship, angleDeg, thrust))





      }
    }
  }
}
