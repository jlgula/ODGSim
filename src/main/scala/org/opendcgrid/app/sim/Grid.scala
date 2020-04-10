package org.opendcgrid.app.sim

import squants.energy.{Energy, Watts}
import squants.time.{Seconds, Time}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/*
  The grid is an acyclic directed graph where the nodes represent physical devices the arcs
  represent electrical connections between devices.

  The grid simulation operates in a loop of time intervals up to a fixed time limit.

  Each time interval is referred to as a tick. During each tick:
    Devices negotiate to establish power constraints and flow direction on each port.
    Based on those constraints, for the duration of the tick, energy is moved between devices.

  At the end of each tick, devices are tested for conditions such as not having enough energy to operate loads.
  The cycle operates at least once per time interval but it may operate multiple times to implement events.

  Devices are nodes in a graph. Each device has n ports. The graph consists of links between ports.
  Each device has at most one link to another device. Ports may be unattached.
  Links are directional where the direction indicates the direction of energy flow.
  Links can change direction during power negotiations but at the end of negotiations, no cycles are permitted.

  Each device can have a power source producing a constrained amount of energy during a tick interval.
  Each device can consume power changing energy to heat during a tick interval.
  Each device can have a battery charging power to store energy and discharging power to reduce energy.
  Any power that is produced but not consumed internally, stored in the battery or sent through ports is lost.
 */
class Grid(
            val devices: Set[Device] = Set(),
            val links: Map[Port, Port] = Map(),
            val configuredEvents: Seq[Event] = Seq()) {

  private val bidirectionalLinks = links ++ mutable.HashMap(links.toSeq.map { z: (Port, Port) => (z._2, z._1) }: _*)
  private val mutableDevices: Set[MutableDevice] = devices.map(_.buildMutableDevice())

  def run(configuration: RunConfiguration = RunConfiguration()): Seq[LogItem] = {

    var timeOffset: Time = Seconds(0) // Time since start of run
    var timeDelta: Time = Seconds(0) // Time since last event
    val ticks = (1 until configuration.tickCount).map(_ * configuration.tickInterval).map(TickEvent)
    val events = mutable.PriorityQueue[Event](configuration.toDo ++ ticks: _*)(Ordering.by { t: Event => t.time }).reverse
    val log = ArrayBuffer[LogItem]()
    var eventCount: Int = 0

    def assignPower(timeDelta: Time): Unit = {
      mutableDevices.foreach { device: MutableDevice => device.updatePowerState(timeDelta, links) }
      moveEnergy(timeDelta)

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
      for (device <- mutableDevices) {
        device.validatePower(timeOffset).foreach(report(ReportSelection.UnderPower, _))
        report(ReportSelection.DeviceStatus, LogItem.Device(timeOffset, device.deviceID, device.uuid, device.consumption, device.production, device.batteryCharge, device.on))
        for (port <- device.ports) {
          report(ReportSelection.PortStatus, LogItem.Port(timeOffset, device.deviceID, port.name, device.portPower(port)))
        }
      }
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
          val sourceDevice = getDevice(message.port)
          report(ReportSelection.PowerMessage, LogItem.Message(timeOffset, sourceDevice.uuid, remoteDevice.uuid, mappedMessage))
        }
      }
    }

    /*
     A terminal node is any node that has no source connection to provide power to other devices.
     A primed node is a node where all source connections have an energy assignment.
     Energy moves in radiating wave from terminal nodes through primed nodes until all nodes have been processed.
     From each terminal node, energy is assigned via load ports to source ports on other nodes.
     As soon as a device has received energy demands on all its source ports, it becomes primed
     and can then be processed, potentially creating new primed nodes.
     */
    def moveEnergy(timeDelta: Time): Unit = {
      val portsWithEnergy = mutable.HashMap[Port, Energy]()
      var remainingDevices = ArrayBuffer(mutableDevices.toSeq: _*)
      var primedDevices: ArrayBuffer[MutableDevice] = ArrayBuffer[MutableDevice]()

      def isSourcePort(device: MutableDevice, port: Port): Boolean = device.portPower(port) < Watts(0)

      def isPrimedDevice(device: MutableDevice): Boolean = {
        val sourcePorts = device.ports.filter(isSourcePort(device, _))
        sourcePorts.forall(portsWithEnergy.contains)
      }

      var iteration = 0
      while (remainingDevices.nonEmpty) {
        iteration += 1
        if (iteration >= Parameters.maxEnergyIterations) fatal("Too many energy iterations")
        val (primed: mutable.ArrayBuffer[MutableDevice], remaining) = remainingDevices.partition(isPrimedDevice)
        primedDevices = primed
        remainingDevices = remaining
        for (device <- primedDevices) {
          val loadPortsWithDemand = device.processDeviceEnergy(timeDelta, portsWithEnergy.toMap)
          val mappedPortsWithDemand = loadPortsWithDemand.map { case (port, energy) => (getLinkPort(port).get, energy) }
          portsWithEnergy ++= mappedPortsWithDemand
        }
      }
    }

    def report(selection: ReportSelection, item: LogItem): Unit = {
      if (configuration.log.contains(selection)) log += item
      if (configuration.trace.contains(selection)) println(item)
    }

    configuration.name.foreach((n: String) => report(ReportSelection.ConfigurationName, LogItem.Configuration(timeOffset, n)))

    // Run through the power loop once to deal with static conditions.
    assignPower(Seconds(0))

    while (events.nonEmpty) {
      val next = events.dequeue()
      eventCount += 1
      if (eventCount >= Parameters.maxEvents) fatal("Event count overflow")
      timeDelta = next.time - timeOffset
      timeOffset = next.time
      next match {
        //case _: QuitEvent => events.clear()
        case _: TickEvent => report(ReportSelection.TickEvent, LogItem.EventItem(next)) // just used to trigger power assignments
        case u: UpdateDeviceState =>
          assignPower(timeDelta) // Make sure the state is up to date before we change production or consumption.
          timeDelta = Seconds(0) // Get ready to rerun assign power at the same time but new configuration.
          mutableDevices.find(_.uuid == u.device).foreach(_.updateState(u.consumption, u.production))
          report(ReportSelection.UpdateDeviceEvent, LogItem.EventItem(next))
      }

      //log += EventLogItem(next)
      //if (configuration.trace) println(traceEvent(next))
      assignPower(timeDelta)

    }

    log
  }


  def mapMessage(message: PowerMessage, targetPort: Port): PowerMessage = message match {
    case p: PowerRequest => PowerRequest(targetPort, p.power)
    case p: PowerGrant => PowerGrant(targetPort, p.power)
  }

  // Gets the device referenced by a port by matching uuid.
  def getDevice(port: Port): MutableDevice = {
    mutableDevices.find(_.uuid == port.uuid).getOrElse(fatal(s"GetDevice failed - port: $port"))
  }

  def getLinkPort(port: Port): Option[Port] = bidirectionalLinks.get(port)

  def fatal(message: String) = throw new IllegalStateException(message)
}
