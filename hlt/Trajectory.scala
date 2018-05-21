package hlt

class Trajectory(val start: Position, val end: Position, val id: Long, val isNew: Boolean = false) {

  //val start = new Position(xPos, yPos)
  //val end = new Position(xPos, yPos)
  val length = start.getDistanceTo(end)

  def getMoveFromTraj(gameMap: GameMap): Option[ThrustMove] = {
    val ship = gameMap.getAllShips
                      .filter(_.owner==Some(gameMap.getMyPlayerId))
                      .filter(_.id == id)
                      .head
    new Navigation(gameMap, ship, end).navigateTo
  }
  def intersects(traj2: Trajectory, collisionTracker: Boolean = false): Boolean = {
    if (traj2 == this) {
      true
    } else {
      val dx = Constants.SHIP_RADIUS * Math.cos(start.orientTowardsInRad(traj2.start))
      val dy = Constants.SHIP_RADIUS * Math.sin(start.orientTowardsInRad(traj2.start))
      val dx2 = Constants.SHIP_RADIUS * Math.cos(end.orientTowardsInRad(traj2.end))
      val dy2 = Constants.SHIP_RADIUS * Math.sin(end.orientTowardsInRad(traj2.end))
      val (a1,a2) = (start.xPos + dx,start.yPos + dy)
      val (b1,b2) = (end.xPos + dx2,end.yPos + dy2)
      val (c1,c2) = (traj2.start.xPos - dx,traj2.start.yPos - dy)
      val (d1,d2) = (traj2.end.xPos - dx2,traj2.end.yPos - dy2)
      val A = new Position(a1,a2)
      val B = new Position(b1,b2)
      val C = new Position(c1,c2)
      val D = new Position(d1,d2)
    //  if (collisionTracker) {
        DebugLog.addLog("dx = " + dx)
        DebugLog.addLog("dy = " + dy)
        DebugLog.addLog("A = " + A)
        DebugLog.addLog("B = " + B)
        DebugLog.addLog("C = " + C)
        DebugLog.addLog("D = " + D)
    //  }
      val isCCW = (ccw(A,C,D) != ccw(B,C,D)) && (ccw(A,B,C) != ccw(A,B,D))
      isCCW
    }
  }
  def collidesWith(traj2: Trajectory): Boolean = {
    if (traj2 == this) {
      true
    } else {
      val dx = Constants.SHIP_RADIUS * Math.cos(start.orientTowardsInRad(traj2.start))
      val dy = Constants.SHIP_RADIUS * Math.sin(start.orientTowardsInRad(traj2.start))
      val dx2 = Constants.SHIP_RADIUS * Math.cos(end.orientTowardsInRad(traj2.end))
      val dy2 = Constants.SHIP_RADIUS * Math.sin(end.orientTowardsInRad(traj2.end))
      val (a1,a2) = (start.xPos + dx,start.yPos + dy)
      val (b1,b2) = (end.xPos + dx2,end.yPos + dy2)
      val (c1,c2) = (traj2.start.xPos - dx,traj2.start.yPos - dy)
      val (d1,d2) = (traj2.end.xPos - dx2,traj2.end.yPos - dy2)
      val A = new Position(a1,a2)
      val B = new Position(b1,b2)
      val C = new Position(c1,c2)
      val D = new Position(d1,d2)
      val R = Constants.FORECAST_FUDGE_FACTOR
      val PxN = (((A.xPos * B.yPos) - (A.yPos * B.xPos)) * (C.xPos - D.xPos)) - (((C.xPos * D.yPos) - (C.yPos * D.xPos)) * (A.xPos - B.xPos))
      val PxD = ((A.xPos - B.xPos) * (C.yPos - D.yPos)) - ((A.yPos - B.yPos) * (C.xPos - D.xPos))
      val PyN = (((A.xPos * B.yPos) - (A.yPos * B.xPos)) * (C.yPos - D.yPos)) - (((C.xPos * D.yPos) - (C.yPos * D.xPos)) * (A.yPos - B.yPos))

      val Px = PxN/PxD
      val Py = PyN/PxD
      val det = new Position(Px,Py)
      val AB = A.getDistanceTo(B)
      val CD = C.getDistanceTo(D)
      val AB1 = A.getDistanceTo(det)
      val CD1 = C.getDistanceTo(det)
      //DebugLog.addLog("AB1 = " + AB1)
      //DebugLog.addLog("CD1 = " + CD1)
      if (((AB1/AB) < ((CD1 - (2*R))/CD)) || ((CD1/CD) < ((AB1 - (2*R))/AB) )){
        false
      } else {
        true
      }
    }
  }


  private def ccw(A: Position, B: Position, C: Position): Boolean = {
    (C.yPos-A.yPos)*(B.xPos-A.xPos) > (B.yPos-A.yPos)*(C.xPos-A.xPos)
  }
  override def toString: String =
    "Trajectory[" + start.toString + "," + end.toString + "," + id + "]"
  def canEqual(a: Any) = a.isInstanceOf[Trajectory]
  override def equals(that: Any): Boolean = that match {
    case that: Trajectory => that.canEqual(this) && this.hashCode == that.hashCode
    case _ => false
  }
  override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + start.hashCode;
    result = prime * result + end.hashCode
    result
  }
}
