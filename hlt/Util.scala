package hlt

object Util {
  def angleRadToDegClipped(angleRad: Double): Int = {
    val degUnclipped = Math.round(Math.toDegrees(angleRad))
    // Make sure return value is in [0, 360) as required by game engine.
    (((degUnclipped % 360L) + 360L) % 360L).toInt
  }
  def angleDegToRad(angleDeg: Int): Double = {
    Math.toRadians(angleDeg)
    //(((degUnclipped % 360L) + 360L) % 360L).toInt
  }

  def mean(a: Iterable[Position]): Position = {
    if (a.size == 1) {
      a.head
    } else {
      val sumX: Double = a.foldLeft(0.0)({ case (b,a) => a.xPos + b })
      val sumY: Double = a.foldLeft(0.0)({ case (b,a) => a.yPos + b })
      val avX = sumX/a.size
      val avY = sumY/a.size
      new Position(avX, avY)
    }
  }
}
