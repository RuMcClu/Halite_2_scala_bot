package hlt

import scala.annotation.tailrec

class CollisionTrajectory(moves: Iterable[Move], gameMap: GameMap) {
  val trajTableOut = moves.flatMap(move => tryTrajectory(move))


  def goGoTrajet: Iterable[Move] = {
    trajTableOut.size match {
      case 0 => Iterable()
      //case 1 => Iterable(trajTableOut.head.getMoveFromTraj(gameMap))
      case _ => {
        val trajTableOut2 = getTrajectories(trajTableOut)
        val movesOut = trajTableOut2.map(traj=>traj.getMoveFromTraj(gameMap))
        movesOut.flatten
      }
    }
  }

  def makeTrajectory(move: ThrustMove): Trajectory = {
    val angleRad = Util.angleDegToRad(move.getAngle)
    val speed = move.getThrust
    val ship = move.getShip
    val start = new Position(ship.xPos, ship.yPos)
    val end = start.getCartFromRad(speed, angleRad)
    new Trajectory(start, end, ship.id)
  }

  def tryTrajectory(move: Move): Option[Trajectory] = {
    move match {
      case a: ThrustMove => Some(makeTrajectory(a))
      case _ => None
    }
  }

  /////RECURSION

  @tailrec
  private def getTrajectories(trajTable: Iterable[Trajectory],
                              collisionTracker: Boolean = false,
                              maxCorrections: Int =  Constants.MAX_CORR_SHIPS ): Iterable[Trajectory] = {
    val trajTF = trajTable.map(traj => needNewMove(traj,trajTable, collisionTracker))
    //DebugLog.addLog("trajT/F = " + trajTF)
    if (trajTF.exists(_ == true) && (maxCorrections >= 0)){

                   val superNearTable = trajTable.map(traj =>
                     trajTable.filter(_.start.getDistanceTo(traj.start) < 1.5))
                                                  .map(group =>
                     if (group.size > 1) distanceCheck(group) else group
                   ).toList.flatten.distinct.groupBy(_.id).values
                    .map(_.reduceLeft(isItNew(_,_))).toSet.toList

                   val nearTable = superNearTable.map(traj =>
                     superNearTable.filter(_.start.getDistanceTo(traj.start) < 14))
                      // .partition(_.size == 1)

                   val interTable = nearTable.map(group =>
                     group.map(traj =>
                       group.filter(_.intersects(traj))
                         ))

                   val collTable = interTable.map(group =>
                     group.map(subGroup =>
                       subGroup.map(traj =>
                         subGroup.filter(_.collidesWith(traj)))
                           ))

                   val newTable = collTable.map(group =>
                     group.map(subGroup=>
                       subGroup.map(subsubGroup =>
                         permuteEndPoints(subsubGroup))))

                    val newTable2= newTable.toList.flatten
                         .toList.flatten
                         .toList.flatten
                    //DebugLog.addLog("new Trajs = " + newTable2)
                    val newTable3= newTable2.distinct
                         .groupBy(_.id)

                    val newTable4 = newTable3.values
                         .map(_.reduceLeft(isItNew(_,_)))
                         .toSet
                         .toList
                   DebugLog.addLog("trajs reduced = "+ newTable4)
                   DebugLog.addLog("about to recurse")
                   val newMoves = newTable4.flatMap(traj=>traj.getMoveFromTraj(gameMap))
                   val newTrajs = newMoves.flatMap(move => tryTrajectory(move))
                   //DebugLog.addLog("after Navigation" + newTrajs)
                   getTrajectories(newTrajs,
                                   true,
                                   maxCorrections - 1)
                 } else {
                   //DebugLog.addLog("outputtrajtable = "+ trajTable)
                   trajTable
                 }
  }
  def isItNew(traj1: Trajectory, traj2: Trajectory): Trajectory = {
    if (traj1.isNew == true) {
      traj1
    } else {
      traj2
    }
  }
  override def toString: String =
    trajTableOut.toString

  private def distanceCheck(group: Iterable[Trajectory]): Iterable[Trajectory] = {
    val moveGroup = group.flatMap(traj=>traj.getMoveFromTraj(gameMap))
    if (moveGroup.exists(_.getThrust!=7)) {
      val meanSpeed = moveGroup.map(_.getThrust).sum / moveGroup.size
      moveGroup.flatMap(move => conform(move, meanSpeed))
    } else {
      group
    }
  }

  private def conform(move: ThrustMove, meanSpeed: Double): Option[Trajectory] = {
    val move2 = new ThrustMove(move.ship,move.angleDeg,Math.round(meanSpeed).toInt)
    tryTrajectory(move2)
  }

  private def needNewMove(traj: Trajectory,
                          trajTableRec: Iterable[Trajectory],
                          collisionTracker: Boolean): Boolean = {
    //trajTableOut = trajTableRec
    //DebugLog.addLog("traj permutation = " + traj.toString)
    val nearTrajs = trajTableRec.filter(_.id != traj.id)
      .filter(_.start.getDistanceTo(traj.start) < 14)
    if (nearTrajs.isEmpty) {
      DebugLog.addLog("nearby empty")
      //traj.getMoveFromTraj(gameMap)
      false
    } else {
      val interTrajs = nearTrajs.filter(_.intersects(traj, collisionTracker))
      if (interTrajs.isEmpty){
        //if (collisionTracker)
        DebugLog.addLog("no intersections")
        //traj.getMoveFromTraj(gameMap)
        false
      } else {
        val collTrajs = interTrajs.filter(_.collidesWith(traj))
        if (collTrajs.isEmpty){
          DebugLog.addLog("no collisions")
          //traj.getMoveFromTraj(gameMap)
          false
        } else {
          DebugLog.addLog("collision!!")
          //DebugLog.addLog("colltraj = " + collTrajs + ", traj = " + traj)
          //val newTrajs = permuteEndPoints(collTrajs, traj)
          //DebugLog.addLog("permutation = " + newTrajs)
          //val newIds = newTrajs.map(traj => traj.id).toSet
          //val traj2 = newTrajs.filter(_.id==traj.id).head
          //val trajTableRec2 = (for {x <- trajTableRec if !newIds.contains(x.id) } yield x) ++ newTrajs
          //getNewMove(traj2, trajTableRec2, gameMap)
          true
        }
      }
    }
  }

  private def permuteEndPoints(collTrajs: Iterable[Trajectory]): Iterable[Trajectory] = {
    if (collTrajs.size>1) {
      DebugLog.addLog("colltraj = " + collTrajs)
      val lenSet = collTrajs.size - 2
      val subSet = collTrajs.take(2)
      val otherSet = collTrajs.takeRight(lenSet)
      val subject1: Trajectory = subSet.head
      val object1: Trajectory = subSet.last
      val dx = Constants.FORECAST_FUDGE_FACTOR * Math.cos(subject1.end.orientTowardsInRad(object1.end))
      val dy = Constants.FORECAST_FUDGE_FACTOR * Math.sin(subject1.end.orientTowardsInRad(object1.end))
      val (x1dx , y1dy) = ((subject1.end.xPos + (2 * dx)), (subject1.end.yPos + (2 * dy)))
      val (x2dx , y2dy) = ((object1.end.xPos - (2 * dx)), (object1.end.yPos - (2 * dy)))
      val B = new Position(x1dx , y1dy)
      val D = new Position(x2dx , y2dy)
      //DebugLog.addLog("dx = " + dx)
      //DebugLog.addLog("dy = " + dy)
      //DebugLog.addLog("B = " + B)
      //DebugLog.addLog("D = " + D)
      val object2 = Iterable(new Trajectory(object1.start, B, object1.id, true))
      val subject2 = Iterable(new Trajectory(subject1.start, D, subject1.id, true))
      //DebugLog.addLog("new object = " + object2)
      //DebugLog.addLog("new subject = " + subject2)
      val outSet = subject2 ++ object2 ++ otherSet
      outSet

    } else {
      collTrajs
    }

  }
}
