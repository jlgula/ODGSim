package org.opendcgrid.app.sim

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks.{break, breakable}
import squants.time.{Seconds, Time}
import squants.energy.{Power, Watts}

object Parameters {
  val maxEvents = 1000 // Maximum number of events processed by Grid.run
  val maxPowerIterations = 1000 // Maximum number of passes through the assign power loop.
}

object ODGSim extends App {
  val input = ""
  val logTry = GridBuilder.build(input).map { g: Grid => g.run(g.configuredEvents) }

}

// Enumeration of port directions
sealed trait Direction

object Direction {

  case object Load extends Direction

  case object Source extends Direction

  case object Bidirectional extends Direction

}

case class Port(uuid: Int, name: String, direction: Direction = Direction.Load)

sealed abstract class Event(val time: Time)

case class QuitEvent(t: Time = Seconds(0)) extends Event(t)

case class TickEvent(t: Time) extends Event(t)

sealed abstract class LogItem(val time: Time)

case class EventLogItem(event: Event) extends LogItem(event.time)

case class UnderPowerLogItem(t: Time, device: String, expected: Power, assigned: Power) extends LogItem(t)

sealed abstract class PowerMessage(val port: Port, val power: Power)

case class PowerRequest(pt: Port, pwr: Power) extends PowerMessage(pt, pwr)

case class PowerGrant(pt: Port, pwr: Power) extends PowerMessage(pt, pwr)

class Grid(
            val devices: Set[Device] = Set(),
            val links: Map[Port, Port] = Map(),
            val configuredEvents: Seq[Event] = Seq()) {

  val bidirectionalLinks = links ++ mutable.HashMap(links.toSeq.map { z: (Port, Port) => (z._2, z._1) }: _*)

  def run(toDo: Seq[Event]): Seq[LogItem] = {

    var timeOffset: Time = Seconds(0) // Time since start of run
    val events = mutable.PriorityQueue[Event](toDo: _*)(Ordering.by { t: Event => t.time }).reverse
    val log = ArrayBuffer[LogItem]()
    //val portsDevices = Map[Port, Device](links.keys.map { port => (port, getDevice(port) )}.toSeq: _*)
    val deviceData = Map[Device, DeviceData](devices.toSeq.map { device => (device, device.createData()) }: _*)

    breakable {
      for (_ <- 0 until Parameters.maxEvents) {
        if (events.isEmpty) break
        val next = events.dequeue()
        val timeDelta = next.time - timeOffset
        timeOffset = next.time

        next match {
          case _: QuitEvent => events.clear()
          case _: TickEvent => // just used to trigger power assignments
        }

        log += EventLogItem(next)
        assignPower(timeDelta, deviceData)

        // Log any device that does not receive its required power.
        for (device <- devices) {
          val data = deviceData(device)
          if (data.internalConsumption > data.assignedInternalConsumption) log += UnderPowerLogItem(timeOffset, device.deviceID, data.internalConsumption, data.assignedInternalConsumption)
        }
      }
    }

    log
  }

  def assignPower(timeDelta: Time, deviceData: Map[Device, DeviceData]): Unit = {
    devices.foreach { device: Device => device.initializePowerCycle(deviceData(device), links) }

    breakable {
      // First process all devices with consumption. May generate request messages.
      devices.filter {
        deviceData(_).internalConsumption > Watts(0)
      }.foreach {
        assignPowerAndProcessMessages(_, deviceData)
      }

      // Now try to resolve all messages.
      for (_ <- 0 until Parameters.maxPowerIterations) {
        val devicesToProcess = devices.filter {
          deviceData(_).pendingMessages.nonEmpty
        }
        println(s"devicesToProcess: $devicesToProcess")
        if (devicesToProcess.isEmpty) break
        devicesToProcess.foreach {
          assignPowerAndProcessMessages(_, deviceData)
        }
      }
    }
  }

  def assignPowerAndProcessMessages(device: Device, deviceData: Map[Device, DeviceData]): Unit = {
    val messages = device.assignPower(deviceData(device))
    for (message <- messages) {
      val (remoteDevice, remotePort) = getLinkedDeviceAndPort(device, message.port)
      deviceData(remoteDevice).pendingMessages += mapMessage(message, remotePort)
    }
  }

  def mapMessage(message: PowerMessage, targetPort: Port): PowerMessage = message match {
    case p: PowerRequest => PowerRequest(targetPort, p.power)
    case p: PowerGrant => PowerGrant(targetPort, p.power)
  }

  def getDevice(port: Port): Device = {
    devices.find(_.uuid == port.uuid).getOrElse(fatal(s"GetDevice failed - port: $port"))
  }

  def getLinkedDeviceAndPort(device: Device, port: Port): (Device, Port) = {
    val linkPort = bidirectionalLinks(port)
    (getDevice(linkPort), linkPort)
  }

  def fatal(message: String) = throw new IllegalStateException(message)
}


class DeviceData {
  var internalConsumption: Power = Watts(0)
  var internalProduction: Power = Watts(0)
  val powerFlow: mutable.HashMap[Port, Power] = mutable.HashMap[Port, Power]() // Current power flow by port. Positive is inbound, negative is outbound
  val portDirections: mutable.HashMap[Port, Direction] = mutable.HashMap[Port, Direction]()

  // These are only valid during an allocation cycle.
  //var remainingInternalProduction: Power = Watts(0)  // Initialized to production at start of cycle
  var assignedInternalConsumption: Power = Watts(0) // Initialized to 0 at start of cycle
  //var pendingPowerDemand: Power = Watts(0)    // Initialized to consumption at start of cycle
  //var pendingPowerGrant: Power = Watts(0) // Initialized to 0 at start of cycle.
  var totalPowerDemand: Power = Watts(0) // Initialized to 0 at start of cycle
  var totalPowerAvailable: Power = Watts(0) // Initialized to production  at start of cycle.
  val requestsPending: mutable.HashMap[Port, Power] = mutable.HashMap[Port, Power]()
  val grantsPending: mutable.HashMap[Port, Power] = mutable.HashMap[Port, Power]()
  val pendingMessages: mutable.Queue[PowerMessage] = mutable.Queue[PowerMessage]() // Power demands from other devices to be processed.
}

class Device(val deviceID: String, val uuid: Int, val ports: Seq[Port], val internalConsumption: Power = Watts(0), val internalProduction: Power = Watts(0)) {
  // This is called once after each event to initialize the data for this device
  def initializePowerCycle(data: DeviceData, links: Map[Port, Port]): Unit = {
    //data.remainingInternalProduction = data.internalProduction
    data.assignedInternalConsumption = Watts(0)
    data.totalPowerDemand = data.internalConsumption
    data.totalPowerAvailable = data.internalProduction
    data.requestsPending.clear()
    data.grantsPending.clear()

    // Assign port directions and validate legal configuration.
    for (sourcePort <- links.keys.filter(ports.contains(_))) {
      sourcePort match {
        case p: Port if p.direction == Direction.Source => data.portDirections += (p -> Direction.Source)
        case p: Port if p.direction == Direction.Load => throw new IllegalStateException(s"port $p is not a source")
        case p: Port if p.direction == Direction.Bidirectional => data.portDirections += (p -> Direction.Source)
      }
    }

    for (destinationPort <- links.values.filter(ports.contains(_))) {
      destinationPort match {
        case p: Port if p.direction == Direction.Source => throw new IllegalStateException(s"port $p is not a load")
        case p: Port if p.direction == Direction.Load => data.portDirections += (p -> Direction.Load)
        case p: Port if p.direction == Direction.Bidirectional => data.portDirections += (p -> Direction.Load)
      }
    }

  }

  // Drain any pending power demands and compute total energy demand as the sum of queued demands.
  // Reduce this by any remaining internal energy.
  // Distribute the remaining demand to source ports.
  def assignPower(data: DeviceData): Seq[PowerMessage] = {
    println(s"assignPower ${this.deviceID} messages: ${data.pendingMessages}")
    val result: mutable.ArrayBuffer[PowerMessage] = mutable.ArrayBuffer[PowerMessage]()

    // First figure out how much power is needed from external sources if any and request that from available source ports or internal production.
    val powerRequests = data.pendingMessages.dequeueAll(_.isInstanceOf[PowerRequest])
    data.totalPowerDemand += powerRequests.map(_.power).fold(Watts(0))((a, b) => a + b)
    powerRequests.foreach { r: PowerMessage => data.requestsPending += (r.port -> r.power) }
    val powerGrants = data.pendingMessages.dequeueAll(_.isInstanceOf[PowerGrant])
    data.totalPowerAvailable += powerGrants.map(_.power).fold(Watts(0))((a, b) => a + b)
    powerGrants.foreach { r: PowerMessage => data.grantsPending += (r.port -> r.power) }
    assert(data.pendingMessages.isEmpty)

    // Deal with internal consumption first
    val assignProductionToConsumption = data.totalPowerAvailable.min(data.internalConsumption - data.assignedInternalConsumption)
    println(s"device: ${this.deviceID} assignProductionToConsumption: $assignProductionToConsumption")
    data.assignedInternalConsumption += assignProductionToConsumption
    data.totalPowerDemand -= assignProductionToConsumption
    //data.remainingInternalProduction -= assignProductionToConsumption

    //val assignGrantToConsumption = data.pendingPowerGrant.min(data.internalConsumption - data.assignedInternalConsumption)
    //data.assignedInternalConsumption += assignGrantToConsumption
    //data.remainingInternalProduction -= assignGrantToConsumption

    if (data.assignedInternalConsumption < data.internalConsumption) {
      // Try to get power from some source
      result ++= createDemandRequest(data)
    }

    // Now deal with remaining demands.
    for ((port, power) <- data.requestsPending.toSeq) {
      if (data.totalPowerAvailable >= power) {
        val grant = PowerGrant(port, power)
        result += grant
        data.totalPowerAvailable -= power
      } else {
        // Try to get power from some source
        result ++= createDemandRequest(data)
      }
    }

    /*
    val externalPowerDemand = if (data.pendingMessages.isEmpty) Watts(0) else data.pendingMessages.filter(_.isInstanceOf[PowerRequest]).map(_.power).reduce((a, b) => a + b)
    val totalPowerDemand = externalPowerDemand + data.internalConsumption
    val internalPowerAllocated = Watts(Math.min(totalPowerDemand to Watts, data.internalProduction to Watts))
    val residualPowerDemand = totalPowerDemand - internalPowerAllocated

    // Then collect grants and either use internally or forward to requestees.
    //data.remainingInternalEnergy -= internalEnergyAllocated
    //data.pendingMessages.clear()
    val loadPorts = filterPorts(data, Direction.Load)
    val externalPowerGrant = if (data.pendingMessages.isEmpty) Watts(0) else data.pendingMessages.filter(_.isInstanceOf[PowerGrant]).map(_.power).reduce((a, b) => a + b)
    if (residualEnergyDemand == WattHours(0)) Map()
    else {
      if (loadPorts.isEmpty) throw new IllegalStateException(s"${this} has $residualEnergyDemand Wh demand but no load ports")
      else {
        // TODO: allocate by droop curve. For now, equal demand on all ports
        val demandPerPort = residualEnergyDemand / loadPorts.size
        Map(loadPorts.map { port => (port, PowerRequest(demandPerPort)) }: _*)
      }
    }
    */
    result

  }

  def createDemandRequest(data: DeviceData): Seq[PowerMessage] = {
    val allLoadPorts = data.portDirections.filter(p => p._2 == Direction.Load).keySet
    val untappedLoadPorts = (allLoadPorts -- data.grantsPending.keys).toSeq
    if (untappedLoadPorts.nonEmpty) {
      val targetPort = untappedLoadPorts.head
      val requestedPower = data.totalPowerDemand
      data.grantsPending += (targetPort -> requestedPower)
      Seq(PowerRequest(targetPort, requestedPower))
    }
    else Seq()
  }


  // Create the initial device data.
  def createData(timeDelta: Time = Seconds(0)): DeviceData = {
    val result = new DeviceData()
    result.internalConsumption = this.internalConsumption
    result.internalProduction = this.internalProduction

    // Set the initial value of all port directions. Bidirectional ports default to load.
    ports.foreach { port => result.portDirections += ((port, if (port.direction == Direction.Bidirectional) Direction.Load else port.direction)) }

    // Default power flow for every port is 0.
    ports.foreach { port => result.powerFlow += ((port, Watts(0))) }

    result
  }

  // Find the current value of all ports in a particular direction.
  def filterPorts(data: DeviceData, direction: Direction): Seq[Port] = ports.filter {
    data.portDirections(_) == direction
  }

  override def toString: String = s"Device($deviceID, $uuid)"

  // Device equality and hashcode are based on uuid.
  def canEqual(other: Any): Boolean = other.isInstanceOf[Device]

  override def hashCode(): Int = uuid.hashCode()

  override def equals(other: Any): Boolean = other match {
    case that: Device => (
      that.canEqual(this)
        && super.equals(that)
        && this.uuid == that.uuid
      )
    case _ => false
  }

}

