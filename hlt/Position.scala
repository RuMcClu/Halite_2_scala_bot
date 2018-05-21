package hlt

class Position(val xPos: Double, val yPos: Double) {
  def getDistanceTo(target: Position): Double = {
    val dx = xPos - target.getXPos
    val dy = yPos - target.getYPos
    Math.sqrt(Math.pow(dx, 2) + Math.pow(dy, 2))
  }

  def orientTowardsInDeg(target: Position): Int = Util.angleRadToDegClipped(orientTowardsInRad(target))

  def getClosestPoint(target: Entity, plusOne: Boolean = false): Position = {

    val MIN_DISTANCE = 2
    val radius = target.radius + MIN_DISTANCE
    val angleRad = if (this.getDistanceTo(target) < 14 ) {
      target match {
        case a: Ship if (a.dockingStatus == Ship.Undocked) => {
          target.orientTowardsInRad(this) + (Math.PI/4)}
        case _ => target.orientTowardsInRad(this)
      }
    } else {
      target.orientTowardsInRad(this)
    }

    val dx = target.getXPos + radius * Math.cos(angleRad)
    val dy = target.getYPos + radius * Math.sin(angleRad)
    new Position(dx, dy)
  }

  def getSafestPoint(target: Position): Position = {
    val MIN_DISTANCE = 30
    val radius = MIN_DISTANCE
    val angleRad = target.orientTowardsInRad(this)
    val dx = target.getXPos + radius * Math.cos(angleRad)
    val dy = target.getYPos + radius * Math.sin(angleRad)
    new Position(dx, dy)
  }

  def getYPos: Double = yPos

  def orientTowardsInRad(target: Position): Double = {
    val dx = target.getXPos - xPos
    val dy = target.getYPos - yPos
    Math.atan2(dy, dx) + 2 * Math.PI
  }

  def getCartFromRad(distance: Int, radians: Double): Position = {
    new Position(xPos + (distance * Math.cos(radians)), (yPos + distance * Math.sin(radians)))
  }

  def getXPos: Double = xPos

  override def toString: String = "Pos(" + xPos.toInt + ", " + yPos.toInt + ")"

  override def equals(other: Any): Boolean = other match {
    case that: Position =>
      (that canEqual this) &&
        xPos == that.xPos &&
        yPos == that.yPos
    case _ => false
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[Position]

  override def hashCode(): Int = {
    val state = Seq(xPos, yPos)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
