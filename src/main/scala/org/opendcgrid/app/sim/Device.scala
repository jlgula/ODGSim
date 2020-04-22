package org.opendcgrid.app.sim

import squants.energy.{Energy, KilowattHours, Power, WattHours, Watts}
import squants.time.{Seconds, Time}
import squants.energy.EnergyConversions.EnergyNumeric
import squants.energy.PowerConversions.PowerNumeric
import squants.market.Price

import scala.collection.mutable

trait Device {
  val deviceID: String // String description for device
  val uuid: Int // Unique ID for device, used in Ports
  val ports: Seq[Port] // All ports on device
  val internalConsumption: Power // Power consumed - default value
  val internalProduction: Power // Power produced - default value
  val battery: Battery // Energy storage, note: can be null ie 0 max charge
  val initialPowerPrice: Price[Energy] // Default power price for device
  val priceStep: Price[Energy] // Amount to raise or lower price to change demand
  val priceMax: Price[Energy] // Maximum value of the power price.
}

trait DeviceBuilder {

  // Generate a mutable device instance from this definition.
  // Separating mutable from immutable permits static definitions that can be reused per run - test cases.
  def buildMutableDevice(links: Map[Port, Port]): MutableDevice
}

trait MutableDevice extends Device {
  // True if the device had enough power to operate during the entire power cycle.
  def on: Boolean

  def on_=(value: Boolean)

  // Power currently being consumed.
  def consumption: Power

  // Power currently being produced.
  def production: Power

  // The price at which the device is willing to buy or sell energy in $/kWh
  def powerPrice: Price[Energy]

  // Generate power requests and grants on various ports.
  // The tick interval determines how much battery power is available to allocate (charge/tick).
  def assignPower(tickInterval: Time): Seq[Message]

  // Add a message to the input message queue.
  def postMessage(message: Message): Unit

  // True iff the input message queue is not empty
  def hasMessagesToProcess: Boolean

  // This is called once after each tick or event to prepare for a sequence of assignPower cycles.
  def initializePowerCycle(timeDelta: Time): Seq[Message]

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

class BasicDevice(
                   val deviceID: String,
                   val uuid: Int,
                   val ports: Seq[Port] = Nil,
                   val internalConsumption: Power = Watts(0),
                   val internalProduction: Power = Watts(0),
                   val battery: Battery = NullBattery,
                   val initialPowerPrice: Price[Energy] = Parameters.powerPrice,
                   val priceStep: Price[Energy] = Parameters.priceStep,
                   val priceMax: Price[Energy] = Parameters.priceMax) extends Device with DeviceBuilder {

  def buildMutableDevice(links: Map[Port, Port]): BasicMutableDevice = {
    val portDirections: mutable.HashMap[Port, Direction] = mutable.HashMap[Port, Direction]() // Direction of connected ports

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

    new BasicMutableDevice(deviceID, uuid, ports, internalConsumption, internalProduction, battery, initialPowerPrice, priceStep, priceMax, portDirections.toMap)
  }

  class BasicMutableDevice(
                            val deviceID: String,
                            val uuid: Int,
                            val ports: Seq[Port],
                            val internalConsumption: Power,
                            val internalProduction: Power,
                            val battery: Battery,
                            val initialPowerPrice: Price[Energy],
                            val priceStep: Price[Energy],
                            val priceMax: Price[Energy],
                            val directions: Map[Port, Direction]) extends MutableDevice {
    var on: Boolean = true
    var batteryCharge: Energy = battery.initialCharge
    var consumption: Power = internalConsumption
    var production: Power = internalProduction
    var powerPrice: Price[Energy] = initialPowerPrice
    val powerFlow: mutable.HashMap[Port, Power] = mutable.HashMap[Port, Power]() // Current power flow by port. Positive is inbound, negative is outbound
    val portDirections: mutable.HashMap[Port, Direction] = mutable.HashMap[Port, Direction](directions.toSeq: _*) // Direction of connected ports
    val portPrices: mutable.HashMap[Port, Price[Energy]] = mutable.HashMap[Port, Price[Energy]]()
    var assignedInternalConsumption: Power = Watts(0) // Power currently assigned to consumption, either the full consumption or none
    val pendingMessages: mutable.Queue[Message] = mutable.Queue[Message]() // Power demands from other devices to be processed.

    def buildMutableDevice(links: Map[Port, Port]): MutableDevice = throw new IllegalStateException("Device is already mutable")

    // This is called once after each event to update the data for this device.
    // This is normally the tick interval and is used to update the state of the battery, if any.
    // It can also be called after other events such as consumption/production change events.
    // These may have a 0 interval.
    def initializePowerCycle(timeDelta: Time): Seq[Message] = {
      // Send a price message to all source ports.
      // This lets loads choose the lowest price source.
      // Send a price message to all load ports.
      // This lets sources select loads with the highest value.
      (allSourcePorts ++ allLoadPorts).filterNot(powerFlow.contains).map(PowerPrice(_, this.powerPrice)).toSeq
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
    def postMessage(message: Message): Unit = pendingMessages += message

    // Called at the end of an assign power cycle to verify that the device has the power it needs.
    def validatePower(time: Time): Option[LogItem] = {
      if (consumption > assignedInternalConsumption) Some(LogItem.UnderPower(time, deviceID, consumption, assignedInternalConsumption)) else None
    }

    // Gets the power flowing through a port.
    // This will be positive for load ports receiving power and negative for source ports supplying power.
    // If a port is not in the powerFlow map, it reports 0.
    def portPower(port: Port): Power = this.powerFlow.getOrElse(port, Watts(0))

    // The sum of all incoming port power minus all outgoing port power.
    private def netPortPowerFlow: Power = powerFlow.values.sum

    // The power available from the battery is the charge divided by the tick interval up to the maximum discharge rate.
    // This potentially understates the battery power as it assumes for for full tick but establishes a lower bound.
    private def batteryPowerAvailable(tickInterval: Time): Power = if (tickInterval == Seconds(0)) Watts(0) else battery.dischargeRate.min(batteryCharge / tickInterval)

    // Process all pending messages and set the port state for all ports.
    // Try to satisfy any internal demand first then grant any left over power to source ports.
    // If there is not enough power to satisfy internal demand or received requests, request power from load ports.
    // With multiple requests, power is granted to the port whose load is offering the highest price.
    // If multiple ports request offering the smae price, requests are granted first come, first served.
    // Each time a new grant is received, all source ports are told to renegotiate via a price message.
    // Price messages set the value of source ports and the cost of load ports.
    // When received on a load port, they also trigger renegotiation on the port as the source's state has changed.
    def assignPower(tickInterval: Time): Seq[Message] = {
      val result: mutable.ArrayBuffer[Message] = mutable.ArrayBuffer[Message]()

      // First figure out how much power is needed from external sources if any and request that from available source ports or internal production.
      val powerRequests = pendingMessages.dequeueAll(_.isInstanceOf[PowerRequest]).collect { case m: PowerRequest => m }
      val powerGrants = pendingMessages.dequeueAll(_.isInstanceOf[PowerGrant]).collect { case m: PowerGrant => m }
      val prices = pendingMessages.dequeueAll(_.isInstanceOf[PowerPrice]).collect { case m: PowerPrice => m }
      assert(pendingMessages.isEmpty)

      // Record prices for ports.
      prices.foreach { pm =>
        if (isLoadPort(pm.port)) powerFlow.remove(pm.port) // Force a renegotiation on port.
        this.portPrices.update(pm.port, pm.price)
      }


      // Reset all outgoing ports with requests since we are going to redo them.
      powerRequests.foreach { r: Message => {
        this.powerFlow.update(r.port, Watts(0))
      }
      }

      // Record power incoming via load ports.
      powerGrants.foreach { r: PowerGrant => {
        allSourcePorts.foreach(result += PowerPrice(_, this.powerPrice)) // Tell all source ports to renegotiate
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

      // Now grant power for requests in order of the price the port willing to pay.
      assert(powerRequests.forall(pr => portPrices.contains(pr.port)))
      val priceSortedRequests = powerRequests.sortBy(pr => portPrices(pr.port) * KilowattHours(1)).reverse
      //val requests = mutable.Queue[PowerRequest](powerRequests: _*)
      val requests = mutable.Queue[PowerRequest](priceSortedRequests: _*)
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
        // Issue requests to ports that have not yet received a grant as defined by no entry in the power flow table.
        // and is affordable relative to what I'm willing to pay.
        result ++= allLoadPorts.filter(port => (!powerFlow.isDefinedAt(port)) && isPortAffordable(port)).map(PowerRequest(_, totalPowerNeeded))
      }

      result
    }

    // A port is affordable for sourcing power if its price is less than my price (interpreted as the price I'm willing to pay).
    def isPortAffordable(port: Port): Boolean = {
      val price = portPrices.getOrElse(port, throw new IllegalStateException(s"Device: ${this.toString}. No price on port $port"))
      (price * KilowattHours(1)) <= (this.powerPrice * KilowattHours(1))
    }

    // This is called at the end of a cycle to tell devices they need to renegotiate power grants.
    def renegotiatePowerAssignments(): Unit = allLoadPorts.foreach {
      powerFlow.remove
    }

    // True iff port is connected and configured to be a load, receiving power from a source.
    def isLoadPort(port: Port): Boolean = portDirections.get(port) match {
      case None => false
      case Some(direction) => direction == Direction.Load
    }

    // The set of all ports connected to a device and configured to be a load.
    def allLoadPorts: collection.Set[Port] = ports.filter(isLoadPort).toSet

    // True iff port is connected and configured to be a source, offering power to a load.
    def isSourcePort(port: Port): Boolean = portDirections.get(port) match {
      case None => false
      case Some(direction) => direction == Direction.Source
    }

    // The set of all ports that are connected to another device and configured to be a source.
    def allSourcePorts: collection.Set[Port] = ports.filter(isSourcePort).toSet

    def updateState(consumption: Power, production: Power): Unit = {
      this.consumption = consumption
      this.production = production
    }
  }

  override def toString: String = s"Device($deviceID, $uuid)"
}

