package org.opendcgrid.app.sim

import squants.energy.{Energy, Power, WattHours, Watts}
import squants.time.{Seconds, Time}
import squants.energy.EnergyConversions.EnergyNumeric
import squants.energy.PowerConversions.PowerNumeric

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
  // True if the device had enough power to operate during the entire power cycle.
  def on: Boolean

  def on_=(value: Boolean)

  // Power currently being consumed.
  def consumption: Power

  // Power currently being produced.
  def production: Power

  // Generate power requests and grants on various ports.
  // The tick interval determines how much battery power is available to allocate (charge/tick).
  def assignPower(tickInterval: Time): Seq[PowerMessage]

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

  // Compute the energy needed by each load port.
  def processDeviceEnergy(timeDelta: Time, portsWithEnergy: Map[Port, Energy]): Seq[(Port, Energy)]

  // Called at the end of the power assignment cycle for devices that do not have the power they need.
  // Tells the device it should renegotiate at the next message cycle.
  def renegotiatePowerAssignments(): Unit
}

// Battery Policy
// The device always tries to get enough power to charge its battery but it will use the battery to fulfill any existing demand.

class BasicDevice(val deviceID: String, val uuid: Int, val ports: Seq[Port] = Nil, val internalConsumption: Power = Watts(0), val internalProduction: Power = Watts(0), val battery: Battery = NullBattery) extends Device {
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

    // Compute the energy needed by each load port.
    def processDeviceEnergy(timeDelta: Time, portsWithEnergy: Map[Port, Energy]): Seq[(Port, Energy)] = {
      val sourcePorts = ports.filter(portPower(_) < Watts(0))
      val loadPorts = ports.filter(portPower(_) > Watts(0))
      val outboundEnergy = sourcePorts.collect(portsWithEnergy).sum
      val batteryEnergyAvailable: Energy = batteryCharge
      val productionEnergy: Energy = production * timeDelta
      val maximumInboundEnergy: Energy = timeDelta * loadPorts.map(portPower).sum
      val maximumAvailableEnergy: Energy = maximumInboundEnergy + productionEnergy + batteryEnergyAvailable - outboundEnergy
      val consumptionEnergyRequired: Energy = consumption * timeDelta
      on = consumptionEnergyRequired <= maximumAvailableEnergy
      val consumedAndOutbound = outboundEnergy + (if (on) consumptionEnergyRequired else WattHours(0))
      var chargeDelta: Energy = WattHours(0)
      var inboundEnergyNeeded: Energy = WattHours(0)
      if (consumedAndOutbound < maximumInboundEnergy + productionEnergy) {
        // Energy available to charge battery.
        val batteryEnergyNeeded: Energy = (battery.capacity - batteryCharge).min(battery.chargeRate * timeDelta)
        val batteryEnergyAvailable = maximumInboundEnergy + productionEnergy - consumedAndOutbound
        chargeDelta = batteryEnergyAvailable.min(batteryEnergyNeeded)
        assert(chargeDelta >= WattHours(0))
        inboundEnergyNeeded = chargeDelta + consumedAndOutbound - productionEnergy
      } else {
        // Must be discharging battery or 0.
        val dischargeEnergy = consumedAndOutbound - (maximumInboundEnergy + productionEnergy)
        assert(dischargeEnergy >= WattHours(0))
        assert(dischargeEnergy <= battery.dischargeRate * timeDelta)
        chargeDelta = -dischargeEnergy
        inboundEnergyNeeded = consumedAndOutbound - (productionEnergy + dischargeEnergy)
        assert(inboundEnergyNeeded <= maximumInboundEnergy)
      }
      assert(batteryCharge + chargeDelta >= WattHours(0))
      batteryCharge += chargeDelta

      // Allocate inbound energy
      // Allocate energy received from attached devices to those devices.
      // Allocation is in proportion to the granted values.
      val totalGrantedPower = loadPorts.map(portPower).sum
      assert(totalGrantedPower >= Watts(0))
      if (totalGrantedPower > Watts(0)) {
        val portFractions = loadPorts.map(portPower(_).toWatts / totalGrantedPower.toWatts)
        val portEnergy = portFractions.map(_ * inboundEnergyNeeded)
        loadPorts.zip(portEnergy)
      } else Nil
    }

    // Called in grid assign power loop to identify devices that need to be processed.
    def needsPower: Boolean = (consumption > Watts(0)) || batteryCharge < battery.capacity

    // Called in grid assign power loop to identify devices with messages that need processing.
    def hasMessagesToProcess: Boolean = pendingMessages.nonEmpty

    // Called to add a message to the message queue.
    def postMessage(message: PowerMessage): Unit = pendingMessages += message

    // Called at the end of an assign power cycle to verify that the device has the power it needs.
    def validatePower(time: Time): Option[LogItem] = {
      if (consumption > assignedInternalConsumption) Some(LogItem.UnderPower(time, deviceID, consumption, assignedInternalConsumption)) else None
    }

    def portPower(port: Port): Power = this.powerFlow(port)

    // The sum of all incoming port power minus all outgoing port power.
    private def netPortPowerFlow: Power = powerFlow.values.sum

    // The power available from the battery is the charge divided by the tick interval up to the maximum discharge rate.
    // This potentially understates the battery power as it assumes for for full tick but establishes a lower bound.
    private def batteryPowerAvailable(tickInterval: Time): Power = if (tickInterval == Seconds(0)) Watts(0) else battery.dischargeRate.min(batteryCharge / tickInterval)

    // Process all pending messages and set the port state for all ports.
    def assignPower(tickInterval: Time): Seq[PowerMessage] = {
      val result: mutable.ArrayBuffer[PowerMessage] = mutable.ArrayBuffer[PowerMessage]()

      // First figure out how much power is needed from external sources if any and request that from available source ports or internal production.
      val powerRequests = pendingMessages.dequeueAll(_.isInstanceOf[PowerRequest])
      val powerGrants = pendingMessages.dequeueAll(_.isInstanceOf[PowerGrant])
      assert(pendingMessages.isEmpty)

      // Reset all outgoing ports with requests since we are going to redo them.
      powerRequests.foreach { r: PowerMessage => {
        this.powerFlow.update(r.port, Watts(0))
      }
      }

      // Record power incoming via load ports.
      powerGrants.foreach { r: PowerMessage => {
        this.powerFlow.update(r.port, r.power)
      }
      }

      // Deal with internal consumption first.
      // Consumption is all or nothing. Either there is enough for all consumption or none is assigned.
      val inboundPower = ports.map(portPower).filter(_ > Watts(0)).sum
      var powerAvailable = inboundPower + production + batteryPowerAvailable(tickInterval)
      assignedInternalConsumption = if (powerAvailable >= consumption) consumption else Watts(0)
      powerAvailable -= assignedInternalConsumption

      // If there is not enough power to satisfy all the current grants,
      // send a reject to each current grant port and zero out the port power.
      val outBoundPorts = ports.filter(portPower(_) < Watts(0))
      val outboundPower = -outBoundPorts.map(portPower).sum
      if (powerAvailable < outboundPower) {
        for (port <- outBoundPorts) {
          result += PowerGrant(port, Watts(0))
          powerFlow.update(port, Watts(0))
        }
      } else powerAvailable -= outboundPower

      // Now grant power for requests, first come, first served.
      val requests = mutable.Queue[PowerMessage](powerRequests: _*)
      while (requests.nonEmpty && powerAvailable > Watts(0)) {
        val nextRequest = requests.dequeue()
        // Grant the sum of available power and what's already been granted. Note portPower will be negative.
        val powerGranted = nextRequest.power.min(powerAvailable)
        result += PowerGrant(nextRequest.port, powerGranted)
        powerAvailable -= powerGranted
        this.powerFlow.update(nextRequest.port, -powerGranted)
      }

      // Reject any remaining requests from ports that have not yet been addressed.
      val unfulfilledRequestPower = requests.map(_.power).sum
      while (requests.nonEmpty) {
        val nextRequest = requests.dequeue()
        // It is possible for multiple requests to come through one port.
        // If we respond to the first one, ignore subsequent requests and do not reject.
        // Only respond to requests for ports that have not yet been assigned.
        if (portPower(nextRequest.port) == Watts(0)) {
          result += PowerGrant(nextRequest.port, Watts(0))
        }
        //result += PowerGrant(nextRequest.port, Watts(0))
        //this.powerFlow.update(nextRequest.port, Watts(0))
      }

      // The power we would like to have from other sources is the sum of our unfulfilled requests
      // plus any consumption that we are not able to provide + any power the battery needs - granted incoming power.
      // The battery needs the min of its charge rate and the amount that would fill it in the tick interval.
      // We only want to request from ports that have not already granted power.
      val consumptionPowerNeeded = consumption - assignedInternalConsumption
      val batteryPowerNeeded = if (tickInterval == Seconds(0)) Watts(0) else battery.chargeRate.min((battery.capacity - batteryCharge) / tickInterval)
      val totalPowerNeeded = unfulfilledRequestPower + consumptionPowerNeeded + batteryPowerNeeded - netPortPowerFlow
      if (totalPowerNeeded > Watts(0)) {
        result ++= allLoadPorts.filter(portPower(_) == Watts(0)).map(PowerRequest(_, totalPowerNeeded))
      }

      result
    }

    def renegotiatePowerAssignments(): Unit = ports.filter(portPower(_) > Watts(0)).foreach {
      powerFlow.update(_, Watts(0))
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

