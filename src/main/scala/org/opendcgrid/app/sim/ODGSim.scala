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

//case class QuitEvent(t: Time = Seconds(0)) extends Event(t)

case class TickEvent(t: Time = Seconds(0)) extends Event(t)

sealed abstract class LogItem(val time: Time)

case class EventLogItem(event: Event) extends LogItem(event.time)

case class UnderPowerLogItem(t: Time, device: String, expected: Power, assigned: Power) extends LogItem(t)

sealed abstract class PowerMessage(val port: Port, val power: Power)

case class PowerRequest(pt: Port, pwr: Power) extends PowerMessage(pt, pwr)

case class PowerGrant(pt: Port, pwr: Power) extends PowerMessage(pt, pwr)

case class RunConfiguration(name: Option[String] = None, trace: Boolean = false)

class Grid(
            val devices: Set[Device] = Set(),
            val links: Map[Port, Port] = Map(),
            val configuredEvents: Seq[Event] = Seq()) {

  private val bidirectionalLinks = links ++ mutable.HashMap(links.toSeq.map { z: (Port, Port) => (z._2, z._1) }: _*)

  def run(toDo: Seq[Event], configuration: RunConfiguration = RunConfiguration()): Seq[LogItem] = {

    var timeOffset: Time = Seconds(0) // Time since start of run
    val events = mutable.PriorityQueue[Event](toDo: _*)(Ordering.by { t: Event => t.time }).reverse
    val log = ArrayBuffer[LogItem]()
    var eventCount: Int = 0

    def assignPower(): Unit = {
      devices.foreach { device: Device => device.initializePowerCycle(links) }

      breakable {
        // First process all devices with consumption. May generate request messages.
        devices.filter {
          _.internalConsumption > Watts(0)
        }.foreach {
          assignPowerAndProcessMessages
        }

        // Now try to resolve all messages.
        for (_ <- 0 until Parameters.maxPowerIterations) {
          val devicesToProcess = devices.filter {
            _.pendingMessages.nonEmpty
          }
          //aprintln(s"devicesToProcess: $devicesToProcess")
          if (devicesToProcess.isEmpty) break
          devicesToProcess.foreach {
            assignPowerAndProcessMessages
          }
        }
      }

      // Log any device that does not receive its required power.
      devices.foreach(checkDevice)
    }

    def assignPowerAndProcessMessages(device: Device): Unit = {
      val messages = device.assignPower()
      for (message <- messages) {
        val (remoteDevice, remotePort) = getLinkedDeviceAndPort(device, message.port)
        val mappedMessage = mapMessage(message, remotePort)
        remoteDevice.pendingMessages += mappedMessage
        if (configuration.trace) println(traceMessage(remoteDevice, mappedMessage))
      }
    }

    def traceMessage(targetDevice: Device, message: PowerMessage): String = {
      s"$timeOffset target: ${targetDevice.deviceID} message: $message"
    }

    // Validate internal state of device, logging error conditions
    def checkDevice(device: Device): Unit = {
      if (device.internalConsumption > device.assignedInternalConsumption) log += UnderPowerLogItem(timeOffset, device.deviceID, device.internalConsumption, device.assignedInternalConsumption)
    }


    configuration.name.foreach(println) // Use for tracing particular tests

    // Run through the power loop once to deal with static conditions.
    assignPower()

    while (events.nonEmpty) {
      val next = events.dequeue()
      eventCount += 1
      if (eventCount >= Parameters.maxEvents) fatal("Event count overflow")
      timeOffset = next.time
      next match {
        //case _: QuitEvent => events.clear()
        case _: TickEvent => // just used to trigger power assignments
      }

      log += EventLogItem(next)
      assignPower()

    }

    log
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


class Device(val deviceID: String, val uuid: Int, val ports: Seq[Port], val initialInternalConsumption: Power = Watts(0), val initialInternalProduction: Power = Watts(0)) {
  var internalConsumption: Power = initialInternalConsumption
  var internalProduction: Power = initialInternalProduction
  val powerFlow: mutable.HashMap[Port, Power] = mutable.HashMap[Port, Power]() // Current power flow by port. Positive is inbound, negative is outbound
  val portDirections: mutable.HashMap[Port, Direction] = mutable.HashMap[Port, Direction]()

  // These are only valid during an allocation cycle.
  var assignedInternalConsumption: Power = Watts(0) // Initialized to 0 at start of cycle
  var totalPowerDemand: Power = Watts(0) // Initialized to 0 at start of cycle
  var totalPowerAvailable: Power = Watts(0) // Initialized to production  at start of cycle.
  val requestsPending: mutable.HashMap[Port, Power] = mutable.HashMap[Port, Power]()
  val grantsPending: mutable.HashMap[Port, Power] = mutable.HashMap[Port, Power]()
  val pendingMessages: mutable.Queue[PowerMessage] = mutable.Queue[PowerMessage]() // Power demands from other devices to be processed.

  // Set the initial value of all port directions. Bidirectional ports default to load.
  ports.foreach { port => portDirections += ((port, if (port.direction == Direction.Bidirectional) Direction.Load else port.direction)) }

  // Default power flow for every port is 0.
  ports.foreach { port => powerFlow += ((port, Watts(0))) }


  // This is called once after each event to initialize the data for this device
  def initializePowerCycle(links: Map[Port, Port]): Unit = {
    assignedInternalConsumption = Watts(0)
    totalPowerDemand = internalConsumption
    totalPowerAvailable = internalProduction
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
    powerGrants.foreach { r: PowerMessage => grantsPending += (r.port -> r.power) }
    assert(pendingMessages.isEmpty)

    // Deal with internal consumption first
    val assignProductionToConsumption = totalPowerAvailable.min(internalConsumption - assignedInternalConsumption)
    //println(s"device: ${this.deviceID} assignProductionToConsumption: $assignProductionToConsumption")
    assignedInternalConsumption += assignProductionToConsumption
    totalPowerDemand -= assignProductionToConsumption

    if (assignedInternalConsumption < internalConsumption) {
      // Try to get power from some source
      result ++= createDemandRequest()
    }

    // Now deal with remaining demands.
    for ((port, power) <- requestsPending.toSeq) {
      if (totalPowerAvailable >= power) {
        val grant = PowerGrant(port, power)
        result += grant
        totalPowerAvailable -= power
      } else {
        // Try to get power from some source
        result ++= createDemandRequest()
      }
    }
    result

  }

  def createDemandRequest(): Seq[PowerMessage] = {
    val allLoadPorts = portDirections.filter(p => p._2 == Direction.Load).keySet
    val untappedLoadPorts = (allLoadPorts -- grantsPending.keys).toSeq
    if (untappedLoadPorts.nonEmpty) {
      val targetPort = untappedLoadPorts.head
      val requestedPower = totalPowerDemand
      grantsPending += (targetPort -> requestedPower)
      Seq(PowerRequest(targetPort, requestedPower))
    }
    else Seq()
  }


  // Find the current value of all ports in a particular direction.
  def filterPorts(direction: Direction): Seq[Port] = ports.filter {
    portDirections(_) == direction
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

