package org.opendcgrid.app.sim

import squants.energy.{Energy, Power, Watts}
import squants.time.Time

import scala.collection.mutable

trait Device {
  val deviceID: String
  val uuid: Int
  val ports: Seq[Port]
  val internalConsumption: Power
  val internalProduction: Power
  val battery: Battery

  def buildMutableDevice(): MutableDevice
}

trait MutableDevice extends Device {
  def on: Boolean

  def on_=(value: Boolean)

  // Power currently being consumed.
  def consumption: Power

  // Power currently being produced.
  def production: Power

  // Generate power requests and grants on various ports
  def assignPower(): Seq[PowerMessage]

  // Add a message to the input message queue.
  def postMessage(message: PowerMessage): Unit

  // True iff the input message queue is not empty
  def hasMessagesToProcess: Boolean

  // This is called once after each event to initialize the data for this device
  def updatePowerState(timeDelta: Time, links: Map[Port, Port]): Unit

  // Called in grid assign power loop to identify devices that need to be processed.
  def needsPower: Boolean

  // Invoked at the end of an assign power cycle to verify that internal consumption needs met.
  def validatePower(time: Time): Option[LogItem]

  // Current charge of the battery, if any
  def batteryCharge: Energy

  def batteryCharge_=(value: Energy)

  // Change the consumption or production state.
  def updateState(consumption: Power = Watts(0), production: Power = Watts(0))

  // Display the current power flow for a port
  def portPower(port: Port): Power
}

// Battery Policy
// The device always tries to get enough power to charge its battery but it will use the battery to fulfill any existing demand.

class BasicDevice(val deviceID: String, val uuid: Int, val ports: Seq[Port], val internalConsumption: Power = Watts(0), val internalProduction: Power = Watts(0), val battery: Battery = NullBattery) extends Device {
  def buildMutableDevice(): BasicMutableDevice = new BasicMutableDevice(deviceID, uuid, ports, internalConsumption, internalProduction, battery)

  class BasicMutableDevice(val deviceID: String, val uuid: Int, override val ports: Seq[Port], val internalConsumption: Power, val internalProduction: Power, val battery: Battery) extends MutableDevice {
    var on: Boolean = true
    var batteryCharge: Energy = battery.initialCharge
    var consumption: Power = internalConsumption
    var production: Power = internalProduction
    val powerFlow: mutable.HashMap[Port, Power] = mutable.HashMap[Port, Power](ports.map((port: Port) => (port, Watts(0))): _*) // Current power flow by port. Positive is inbound, negative is outbound
    val portDirections: mutable.HashMap[Port, Direction] = mutable.HashMap[Port, Direction]() // Direction of connected ports
    var assignedInternalConsumption: Power = Watts(0) // Power currently assigned to consumption, either the full consumption or none
    val pendingMessages: mutable.Queue[PowerMessage] = mutable.Queue[PowerMessage]() // Power demands from other devices to be processed.

    def buildMutableDevice(): MutableDevice = this

    // This is called once after each event to update the data for this device.
    // This is normally the tick interval and is used to update the state of the battery, if any.
    // It can also be called after other events such as consumption/production change events.
    // These may have a 0 interval.
    def updatePowerState(timeDelta: Time, links: Map[Port, Port]): Unit = {
      // Assign port directions and validate legal configuration.
      for (sourcePort <- links.keys.filter(ports.contains(_))) {
        sourcePort match {
          case p: Port if p.direction == Direction.Source => portDirections += (p -> Direction.Source)
          case p: Port if p.direction == Direction.Load => throw new IllegalStateException(s"port $p is not a source")
          case p: Port if p.direction == Direction.Bidirectional => portDirections += (p -> Direction.Source)
        }
      }

      for (destinationPort <- links.values.filter(ports.contains(_))) {
        destinationPort match {
          case p: Port if p.direction == Direction.Source => throw new IllegalStateException(s"port $p is not a load")
          case p: Port if p.direction == Direction.Load => portDirections += (p -> Direction.Load)
          case p: Port if p.direction == Direction.Bidirectional => portDirections += (p -> Direction.Load)
        }
      }
    }

    // Called in grid assign power loop to identify devices that need to be processed.
    def needsPower: Boolean = (consumption > Watts(0)) || batteryCharge < battery.capacity

    // Called in grid assign power loop to identify devices with messages that need processing.
    def hasMessagesToProcess: Boolean = pendingMessages.nonEmpty

    // Called to add a message to the message queue.
    def postMessage(message: PowerMessage): Unit = pendingMessages += message

    // Called at the end of an assign power cycle to verify that the device has the power it needs.
    def validatePower(time: Time): Option[LogItem] = {
      if (consumption > assignedInternalConsumption) Some(UnderPowerLogItem(time, deviceID, consumption, assignedInternalConsumption)) else None
    }

    def portPower(port: Port): Power = this.powerFlow(port)

    // The sum of all incoming port power minus all outgoing port power.
    private def netPortPowerFlow: Power = powerFlow.values.fold(Watts(0))((a, b) => a + b)

    // The power available from the battery is the charge divided by the tick interval up to the maximum discharge rate.
    // This potentially understates the battery power as it assumes for for full tick but establishes a lower bound.
    private def batteryPowerAvailable: Power = battery.dischargeRate.min(batteryCharge / Parameters.tickInterval)

    // Process all pending messages and set the port state for all ports.
    def assignPower(): Seq[PowerMessage] = {
      val result: mutable.ArrayBuffer[PowerMessage] = mutable.ArrayBuffer[PowerMessage]()

      // First figure out how much power is needed from external sources if any and request that from available source ports or internal production.
      val powerRequests = pendingMessages.dequeueAll(_.isInstanceOf[PowerRequest])
      val powerGrants = pendingMessages.dequeueAll(_.isInstanceOf[PowerGrant])

      // Record power incoming via load ports.
      powerGrants.foreach { r: PowerMessage => {
        this.powerFlow.update(r.port, r.power)
      }
      }
      assert(pendingMessages.isEmpty)

      // Deal with internal consumption first.
      // Consumption is all or nothing. Either there is enough for all consumption or none is assigned.
      var powerAvailable = netPortPowerFlow + production + batteryPowerAvailable
      assignedInternalConsumption = if (powerAvailable >= consumption) consumption else Watts(0)
      powerAvailable -= assignedInternalConsumption

      // Now grant power for requests, first come, first served.
      val requests = mutable.Queue[PowerMessage](powerRequests: _*)
      while (requests.nonEmpty && powerAvailable > Watts(0)) {
        val nextRequest = requests.dequeue()
        val powerGranted = nextRequest.power.min(powerAvailable)
        result += PowerGrant(nextRequest.port, powerGranted)
        powerAvailable -= powerGranted
        this.powerFlow.update(nextRequest.port, -powerGranted)
      }

      // Reject any remaining requests
      val unfulfilledRequestPower = requests.map(_.power).fold(Watts(0))((a, b) => a + b)
      while (requests.nonEmpty) {
        val nextRequest = requests.dequeue()
        result += PowerGrant(nextRequest.port, Watts(0))
        this.powerFlow.update(nextRequest.port, Watts(0))
      }

      // The power we would like to have from other sources is the sum of our unfulfilled requests
      // plus any consumption that we are not able to provide + any power the battery needs - granted incoming power.
      // The battery needs the min of its charge rate and the amount that would fill it in the tick interval.
      // We only want to request from ports that have not already granted power.
      val consumptionPowerNeeded = consumption - assignedInternalConsumption
      val batteryPowerNeeded = battery.chargeRate.min((battery.capacity - batteryCharge) / Parameters.tickInterval)
      val totalPowerNeeded = unfulfilledRequestPower + consumptionPowerNeeded + batteryPowerNeeded - netPortPowerFlow
      if (totalPowerNeeded > Watts(0)) {
        result ++= allLoadPorts.filter(portPower(_) == Watts(0)).map(PowerRequest(_, totalPowerNeeded))
      }

      result
    }

    def allLoadPorts: collection.Set[Port] = portDirections.filter(p => p._2 == Direction.Load).keySet

    def allSourcePorts: collection.Set[Port] = portDirections.filter(p => p._2 == Direction.Source).keySet

    // Find the current value of all ports in a particular direction.
    def filterPorts(direction: Direction): Seq[Port] = ports.filter {
      portDirections(_) == direction
    }

    def updateState(consumption: Power, production: Power): Unit = {
      this.consumption = consumption
      this.production = production
    }
  }

  override def toString: String = s"Device($deviceID, $uuid)"
}

