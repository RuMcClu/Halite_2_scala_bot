package hlt

class Planet(override val owner: Option[Short],
             override val id: Long,
             override val xPos: Double,
             override val yPos: Double,
             override val health: Short,
             override val radius: Double,
             val dockingSpots: Short,
             val currentProduction: Short,
             val remainingProduction: Short,
             val dockedShips: Seq[Long])
    extends Entity(owner, id, xPos, yPos, health, radius) {
  def isFull: Boolean = dockedShips.size == dockingSpots

  def isOwned: Boolean = owner.isDefined

  def isInPeril(gameMap: GameMap): Boolean = {
    val (myShips, theirShips) = {
      gameMap.getAllShips.filter(_.dockingStatus == Ship.Undocked)
                         .filter(_.getDistanceTo(this) < 15)
                         .partition(_.owner == Some(gameMap.getMyPlayerId))
    }
    myShips.size == 0 && theirShips.size > 0
  }

  override def toString: String =
    "Planet[" + super.toString + ", remainingProduction=" + remainingProduction + ", currentProduction=" +
      currentProduction + ", dockingSpots=" + dockingSpots + ", dockedShips=" + dockedShips + "]"
}
