package org.opendcgrid.app.sim

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import squants.time.{Seconds, Time}
import squants.energy.{Power, Watts}

object Parameters {
  val maxEvents = 1000 // Maximum number of events processed by Grid.run
  val maxPowerIterations = 1000 // Maximum number of passes through the assign power loop.
}

object ODGSim extends App {
  val input = ""
  val logTry = GridBuilder.build(input).map { g: Grid => g.run(RunConfiguration(g.configuredEvents)) }
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

case class RunConfiguration(toDo: Seq[Event] = Nil, name: Option[String] = None, trace: Boolean = false)

class Grid(
            val devices: Set[Device] = Set(),
            val links: Map[Port, Port] = Map(),
            val configuredEvents: Seq[Event] = Seq()) {

  private val bidirectionalLinks = links ++ mutable.HashMap(links.toSeq.map { z: (Port, Port) => (z._2, z._1) }: _*)
  private val mutableDevices: Set[MutableDevice] = devices.map(_.buildMutableDevice())

  def run(configuration: RunConfiguration = RunConfiguration()): Seq[LogItem] = {

    var timeOffset: Time = Seconds(0) // Time since start of run
    val events = mutable.PriorityQueue[Event](configuration.toDo: _*)(Ordering.by { t: Event => t.time }).reverse
    val log = ArrayBuffer[LogItem]()
    var eventCount: Int = 0

    def assignPower(): Unit = {
      mutableDevices.foreach { device: MutableDevice => device.initializePowerCycle(links) }

      var powerIteration = 0

      // First process all devices with consumption
      var devicesToProcess = mutableDevices.filter {
        _.needsPower
      }
      devicesToProcess.foreach {
        assignPowerAndProcessMessages
      }

      // Then try to resolve all messages.
      do {
        devicesToProcess = mutableDevices.filter {
          _.hasMessagesToProcess
        }
        devicesToProcess.foreach {
          assignPowerAndProcessMessages
        }
        powerIteration += 1
        if (powerIteration >= Parameters.maxPowerIterations) fatal("Too many power iterations")
      } while (devicesToProcess.nonEmpty)

      // Log any device that does not receive its required power.
      mutableDevices.foreach(_.validatePower(timeOffset).map(log += _))
    }

    def assignPowerAndProcessMessages(device: MutableDevice): Unit = {
      val messages = device.assignPower()
      for (message <- messages) {
        // If there is something attached to the port, forward to remote device.
        assert(getLinkPort(message.port).isDefined) // Device should not send messages on unattached ports
        getLinkPort(message.port).foreach { remotePort: Port =>
          val remoteDevice = getDevice(remotePort)
          val mappedMessage = mapMessage(message, remotePort)
          remoteDevice.postMessage(mappedMessage)
          if (configuration.trace) println(traceMessage(remoteDevice, mappedMessage))
        }
      }
    }

    def traceMessage(targetDevice: Device, message: PowerMessage): String = {
      s"$timeOffset target: ${targetDevice.deviceID} message: $message"
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

  def getDevice(port: Port): MutableDevice = {
    mutableDevices.find(_.uuid == port.uuid).getOrElse(fatal(s"GetDevice failed - port: $port"))
  }

  def getLinkPort(port: Port): Option[Port] = bidirectionalLinks.get(port)

  def fatal(message: String) = throw new IllegalStateException(message)
}

trait Device {
  val deviceID: String
  val uuid: Int
  val ports: Seq[Port]
  val internalConsumption: Power
  val internalProduction: Power

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
}

class BasicDevice(val deviceID: String, val uuid: Int, val ports: Seq[Port], val internalConsumption: Power = Watts(0), val internalProduction: Power = Watts(0)) extends Device {
  def buildMutableDevice(): BasicMutableDevice = new BasicMutableDevice(deviceID, uuid, ports, internalConsumption, internalProduction)

  class BasicMutableDevice(val deviceID: String, val uuid: Int, override val ports: Seq[Port], val internalConsumption: Power, val internalProduction: Power) extends MutableDevice {
    val powerFlow: mutable.HashMap[Port, Power] = mutable.HashMap[Port, Power]() // Current power flow by port. Positive is inbound, negative is outbound
    val portDirections: mutable.HashMap[Port, Direction] = mutable.HashMap[Port, Direction]() // Direction of connected ports

    // These are only valid during an allocation cycle.
    var assignedInternalConsumption: Power = Watts(0) // Initialized to 0 at start of cycle
    var totalPowerDemand: Power = Watts(0) // Initialized to 0 at start of cycle
    var totalPowerAvailable: Power = Watts(0) // Initialized to production  at start of cycle.
    val requestsPending: mutable.HashMap[Port, Power] = mutable.HashMap[Port, Power]()
    val grantsPending: mutable.HashMap[Port, Power] = mutable.HashMap[Port, Power]()
    val pendingMessages: mutable.Queue[PowerMessage] = mutable.Queue[PowerMessage]() // Power demands from other devices to be processed.

    // Default power flow for every port is 0.
    ports.foreach { port => powerFlow += ((port, Watts(0))) }

    def buildMutableDevice(): MutableDevice = this

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

    // Called in grid assign power loop to identify devices that need to be processed.
    def needsPower: Boolean = internalConsumption > Watts(0)

    // Called in grid assign power loop to identify devices with messages that need processing.
    def hasMessagesToProcess: Boolean = pendingMessages.nonEmpty

    // Called to add a message to the message queue.
    def postMessage(message: PowerMessage): Unit = pendingMessages += message

    // Called at the end of an assign power cycle to verify that the device has the power it needs.
    def validatePower(time: Time): Option[LogItem] = {
      if (internalConsumption > assignedInternalConsumption) Some(UnderPowerLogItem(time, deviceID, internalConsumption, assignedInternalConsumption)) else None
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
        for (_ <- untappedLoadPorts) {
          result ++= createDemandRequest()
        }
      }

      // Now deal with remaining demands.
      // Grant partial power as available.
      for ((port, power) <- requestsPending.toSeq) {
        if (totalPowerAvailable > Watts(0)) {
          val grant = PowerGrant(port, power)
          result += grant
          totalPowerAvailable -= power
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


    // Find the current value of all ports in a particular direction.
    def filterPorts(direction: Direction): Seq[Port] = ports.filter {
      portDirections(_) == direction
    }

  }

  override def toString: String = s"Device($deviceID, $uuid)"
}

