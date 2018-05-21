package hlt

object Ship {

  def values: List[DockingStatus] = List(Undocked, Docking, Docked, Undocking)

  sealed trait DockingStatus

  case object Undocked extends DockingStatus

  case object Docking extends DockingStatus

  case object Docked extends DockingStatus

  case object Undocking extends DockingStatus



  def values2: List[OrderStatus] = List(Settle, Attack, Waiting)

  sealed trait OrderStatus

  case object Settle extends OrderStatus

  case object Attack extends OrderStatus

  case object Hide extends OrderStatus

  case object Waiting extends OrderStatus

  case object Defend extends OrderStatus

}

class Ship(override val owner: Option[Short],
           override val id: Long,
           override val xPos: Double,
           override val yPos: Double,
           override val health: Short,
           val dockingStatus: Ship.DockingStatus,
           val dockedPlanet: Long,
           val dockingProgress: Short,
           val weaponCooldown: Short,
           var orderStatus: Ship.OrderStatus = Ship.Waiting)
    extends Entity(owner, id, xPos, yPos, health, Constants.SHIP_RADIUS) {

  def isEven: Boolean = id % 2 == 0
  def isOdd: Boolean = !isEven

  def isClose(planet: Planet): Boolean =
    getDistanceTo(planet) <= Constants.DOCK_RADIUS + planet.radius

  def canDock(planet: Planet, myID: Option[Short]): Boolean =
    if (getDistanceTo(planet) <= Constants.DOCK_RADIUS + planet.radius){
      if (planet.owner == myID || (planet.owner == None) ){
        if(!planet.isFull){
          true
        } else {
          false
        }
      } else {
        false
      }
    } else {
      false
    }

  override def toString: String =
    "Ship[" + super.toString +", orderStatus=" + orderStatus + ", dockingStatus=" + dockingStatus + ", dockedPlanet=" + dockedPlanet +
      ", dockingProgress=" + dockingProgress + ", weaponCooldown=" + weaponCooldown + "]"
}
