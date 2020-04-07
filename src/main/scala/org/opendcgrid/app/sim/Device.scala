package org.opendcgrid.app.sim

import squants.energy.{Energy, Power, WattHours, Watts}
import squants.time.Time

import scala.collection.mutable

trait Device {
  val deviceID: String
  val uuid: Int
  val ports: Seq[Port]
  val internalConsumption: Power
  val internalProduction: Power
  val battery: Option[Battery]

  def buildMutableDevice(): MutableDevice
}

trait MutableDevice extends Device {
  // Generate power requests and grants on various ports
  def assignPower(): Seq[PowerMessage]

  // Add a message to the input message queue.
  def postMessage(message: PowerMessage): Unit

  // True iff the input message queue is not empty
  def hasMessagesToProcess: Boolean

  // This is called once after each event to initialize the data for this device
  def initializePowerCycle(links: Map[Port, Port]): Unit

  // Called in grid assign power loop to identify devices that need to be processed.
  def needsPower: Boolean

  // Invoked at the end of an assign power cycle to verify that internal consumption needs met.
  def validatePower(time: Time): Option[LogItem]

  // Current charge of the battery, if any
  def batteryCharge: Energy

  // Change the consumption or production state.
  def updateState(consumption: Power = Watts(0), production: Power = Watts(0))

  // Display the current power flow for a port
  def portPower(port: Port): Power
}

// Battery Policy
// The device always tries to get enough power to charge its battery but it will use the battery to fulfill any existing demand.

class BasicDevice(val deviceID: String, val uuid: Int, val ports: Seq[Port], val internalConsumption: Power = Watts(0), val internalProduction: Power = Watts(0), val battery: Option[Battery] = None) extends Device {
  def buildMutableDevice(): BasicMutableDevice = new BasicMutableDevice(deviceID, uuid, ports, internalConsumption, internalProduction, battery)

  class BasicMutableDevice(val deviceID: String, val uuid: Int, override val ports: Seq[Port], val internalConsumption: Power, val internalProduction: Power, val battery: Option[Battery] = None) extends MutableDevice {
    var batteryCharge: Energy = WattHours(0)
    var consumption: Power = internalConsumption
    var production: Power = internalProduction
    val powerFlow: mutable.HashMap[Port, Power] = mutable.HashMap[Port, Power]() // Current power flow by port. Positive is inbound, negative is outbound
    val portDirections: mutable.HashMap[Port, Direction] = mutable.HashMap[Port, Direction]() // Direction of connected ports

    // These are only valid during an allocation cycle.
    var batteryFlow: Power = Watts(0) // Power flow into or out of the battery. Inbound is negative, outbound is positive.
    var assignedInternalConsumption: Power = Watts(0) // Initialized to 0 at start of cycle
    var totalPowerDemand: Power = Watts(0) // Initialized to 0 at start of cycle
    var totalPowerAvailable: Power = Watts(0) // Initialized to production  at start of cycle.
    val requestsPending: mutable.HashMap[Port, Power] = mutable.HashMap[Port, Power]()
    val grantsPending: mutable.HashMap[Port, Power] = mutable.HashMap[Port, Power]()
    val pendingMessages: mutable.Queue[PowerMessage] = mutable.Queue[PowerMessage]() // Power demands from other devices to be processed.

    // Default power flow for every port is 0.
    //ports.foreach { port => powerFlow += ((port, Watts(0))) }

    def buildMutableDevice(): MutableDevice = this

    // This is called once after each event to initialize the data for this device
    def initializePowerCycle(links: Map[Port, Port]): Unit = {
      assignedInternalConsumption = Watts(0)
      totalPowerDemand = consumption
      totalPowerAvailable = production
      ports.foreach { port => powerFlow += ((port, Watts(0))) } // Default power flow for every port is 0.
      requestsPending.clear()
      grantsPending.clear()

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
    def needsPower: Boolean = consumption > Watts(0)

    // Called in grid assign power loop to identify devices with messages that need processing.
    def hasMessagesToProcess: Boolean = pendingMessages.nonEmpty

    // Called to add a message to the message queue.
    def postMessage(message: PowerMessage): Unit = pendingMessages += message

    // Called at the end of an assign power cycle to verify that the device has the power it needs.
    def validatePower(time: Time): Option[LogItem] = {
      if (consumption > assignedInternalConsumption) Some(UnderPowerLogItem(time, deviceID, consumption, assignedInternalConsumption)) else None
    }

    def portPower(port: Port): Power = this.powerFlow(port)

    // Drain any pending power demands and compute total energy demand as the sum of queued demands.
    // Reduce this by any remaining internal energy.
    // Distribute the remaining demand to source ports.
    def assignPower(): Seq[PowerMessage] = {
      //println(s"assignPower ${this.deviceID} messages: ${data.pendingMessages}")
      val result: mutable.ArrayBuffer[PowerMessage] = mutable.ArrayBuffer[PowerMessage]()

      // First figure out how much power is needed from external sources if any and request that from available source ports or internal production.
      val powerRequests = pendingMessages.dequeueAll(_.isInstanceOf[PowerRequest])
      totalPowerDemand += powerRequests.map(_.power).fold(Watts(0))((a, b) => a + b)
      powerRequests.foreach { r: PowerMessage => requestsPending += (r.port -> r.power) }
      val powerGrants = pendingMessages.dequeueAll(_.isInstanceOf[PowerGrant])
      totalPowerAvailable += powerGrants.map(_.power).fold(Watts(0))((a, b) => a + b)
      assert(totalPowerAvailable >= Watts(0))
      powerGrants.foreach { r: PowerMessage => {
        grantsPending += (r.port -> r.power)
        this.powerFlow.update(r.port, r.power)
      }
      }
      assert(pendingMessages.isEmpty)

      // Deal with internal consumption first
      val assignProductionToConsumption = totalPowerAvailable.min(consumption - assignedInternalConsumption)
      assert(assignProductionToConsumption >= Watts(0))
      //println(s"device: ${this.deviceID} assignProductionToConsumption: $assignProductionToConsumption")
      assignedInternalConsumption += assignProductionToConsumption
      assert(assignedInternalConsumption >= Watts(0))
      totalPowerDemand -= assignProductionToConsumption
      assert(totalPowerDemand >= Watts(0))

      if (assignedInternalConsumption < consumption) {
        // Try to get power from some source
        for (_ <- untappedLoadPorts) {
          result ++= createDemandRequest()
        }
      }

      // Now deal with remaining demands.
      // Grant partial power as available.
      for ((port, power) <- requestsPending.toSeq) {
        if (totalPowerAvailable > Watts(0)) {
          val powerGranted = power.min(totalPowerAvailable) // Grant the min of request and power available
          val grant = PowerGrant(port, powerGranted)
          result += grant
          totalPowerAvailable -= powerGranted
          this.powerFlow.update(port, -powerGranted)
        } else {
          // Try to get power from some source
          for (_ <- untappedLoadPorts) {
            result ++= createDemandRequest()
          }
        }
      }
      result

    }

    def allLoadPorts: collection.Set[Port] = portDirections.filter(p => p._2 == Direction.Load).keySet

    def allSourcePorts: collection.Set[Port] = portDirections.filter(p => p._2 == Direction.Source).keySet

    def untappedLoadPorts: Seq[Port] = (allLoadPorts -- grantsPending.keys).toSeq

    def createDemandRequest(): Seq[PowerMessage] = {
      //val allLoadPorts = portDirections.filter(p => p._2 == Direction.Load).keySet
      //val untappedLoadPorts = (allLoadPorts -- grantsPending.keys).toSeq
      if (untappedLoadPorts.nonEmpty) {
        val targetPort = untappedLoadPorts.head
        val requestedPower = totalPowerDemand
        grantsPending += (targetPort -> requestedPower)
        Seq(PowerRequest(targetPort, requestedPower))
      }
      else Seq()
    }

    def batteryDemand: Power = {
      if (battery.isEmpty || batteryCharge >= battery.get.capacity) Watts(0)
      else battery.get.chargeRate
    }

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

