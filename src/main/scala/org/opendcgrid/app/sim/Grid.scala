package org.opendcgrid.app.sim

import squants.energy.{Energy, WattHours, Watts}
import squants.time.{Seconds, Time}
import squants.energy.EnergyConversions.EnergyNumeric
import squants.energy.PowerConversions.PowerNumeric

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
    val ticks = (1 until Parameters.maxTicks).map(Seconds(_)).map(TickEvent)
    val events = mutable.PriorityQueue[Event](configuration.toDo ++ ticks: _*)(Ordering.by { t: Event => t.time }).reverse
    val log = ArrayBuffer[LogItem]()
    var eventCount: Int = 0

    def traceMessage(sourceDevice: Device, targetDevice: Device, message: PowerMessage): String = {
      s"$timeOffset message source: ${sourceDevice.deviceID} target: ${targetDevice.deviceID} message: $message"
    }

    def traceEvent(event: Event): String = s"$timeOffset event: $event"

    def traceDevicePort(device: MutableDevice, port: Port): String = {
      s"$timeOffset port: ${port.name}, power: ${device.portPower(port)}"
    }

    def traceLog(item: LogItem): String = s"$timeOffset logItem: $item"

    def traceDevice(device: MutableDevice): String = s"$timeOffset device: ${device.deviceID}:${device.uuid} consumption: ${device.consumption} production: ${device.production} charge: ${device.batteryCharge} on: ${device.on}"


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
        val result = device.validatePower(timeOffset)
        if (result.isDefined) {
          val item = result.get
          log += item
          if (configuration.trace) {
            println(traceLog(item))
          }
        }
      }

      if (configuration.trace) {
        for (device <- mutableDevices) {
          println(traceDevice(device))
          for (port <- device.ports) {
            println(traceDevicePort(device, port))
          }
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
          if (configuration.trace) println(traceMessage(sourceDevice, remoteDevice, mappedMessage))
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

      def processDeviceEnergy(device: MutableDevice): Unit = {
        val sourcePorts = device.ports.filter(isSourcePort(device, _))
        val loadPorts = device.ports.filterNot(isSourcePort(device, _))
        val outboundEnergy = sourcePorts.collect(portsWithEnergy).sum
        val batteryEnergyAvailable: Energy = device.batteryCharge
        val productionEnergy: Energy = device.production * timeDelta
        val maximumInboundEnergy: Energy = timeDelta * loadPorts.map(device.portPower).sum
        val maximumAvailableEnergy: Energy = maximumInboundEnergy + productionEnergy + batteryEnergyAvailable - outboundEnergy
        val consumptionEnergyRequired: Energy = device.consumption * timeDelta
        device.on = consumptionEnergyRequired <= maximumAvailableEnergy
        val consumedAndOutbound = outboundEnergy + (if (device.on) consumptionEnergyRequired else WattHours(0))
        var chargeDelta: Energy = WattHours(0)
        var inboundEnergyNeeded: Energy = WattHours(0)
        if (consumedAndOutbound < maximumInboundEnergy + productionEnergy) {
          // Energy available to charge battery.
          val batteryEnergyNeeded: Energy = batteryChargeEnergy(device)
          val batteryEnergyAvailable = maximumInboundEnergy + productionEnergy - consumedAndOutbound
          chargeDelta = batteryEnergyAvailable.min(batteryEnergyNeeded)
          assert(chargeDelta >= WattHours(0))
          inboundEnergyNeeded = chargeDelta + consumedAndOutbound - productionEnergy
        } else {
          // Must be discharging battery or 0.
          val dischargeEnergy = consumedAndOutbound - (maximumInboundEnergy + productionEnergy)
          assert(dischargeEnergy >= WattHours(0))
          assert(dischargeEnergy <= device.battery.dischargeRate * timeDelta)
          chargeDelta = -dischargeEnergy
          inboundEnergyNeeded = consumedAndOutbound - (productionEnergy + dischargeEnergy)
          assert(inboundEnergyNeeded <= maximumInboundEnergy)
        }
        device.batteryCharge += chargeDelta
        allocateInboundEnergy(device, inboundEnergyNeeded)
      }

      // Allocate energy received from attached devices to those devices.
      // Allocation is in proportion to the granted values.
      def allocateInboundEnergy(device: MutableDevice, energy: Energy): Unit = {
        val loadPorts = device.ports.filter(isLoadPort(device, _))
        val totalGrantedPower = loadPorts.map(device.portPower).sum
        assert(totalGrantedPower >= Watts(0))
        if (totalGrantedPower > Watts(0)) {
          val portFractions = loadPorts.map(device.portPower(_).toWatts / totalGrantedPower.toWatts)
          val portEnergy = portFractions.map(_ * energy)
          val mappedLoadPorts: Seq[Port] = loadPorts.map(getLinkPort(_).get)
          val pairs = mappedLoadPorts.zip(portEnergy)
          portsWithEnergy ++= pairs
        }
      }

      def isSourcePort(device: MutableDevice, port: Port): Boolean = device.portPower(port) < Watts(0)

      def isLoadPort(device: MutableDevice, port: Port): Boolean = device.portPower(port) > Watts(0)

      def isPrimedDevice(device: MutableDevice): Boolean = {
        val sourcePorts = device.ports.filter(isSourcePort(device, _))
        sourcePorts.forall(portsWithEnergy.contains)
      }

      def batteryChargeEnergy(device: MutableDevice): Energy = (device.battery.capacity - device.batteryCharge).min(device.battery.chargeRate * timeDelta)

      var iteration = 0
      while (remainingDevices.nonEmpty) {
        iteration += 1
        if (iteration >= Parameters.maxEnergyIterations) fatal("Too many energy iterations")
        val (primed: mutable.ArrayBuffer[MutableDevice], remaining) = remainingDevices.partition(isPrimedDevice)
        primedDevices = primed
        remainingDevices = remaining
        primedDevices.foreach(processDeviceEnergy)

      }
    }


    configuration.name.foreach(println) // Use for tracing particular tests

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
        case _: TickEvent => // just used to trigger power assignments
        case u: UpdateDeviceState =>
          assignPower(timeDelta) // Make sure the state is up to date before we change production or consumption.
          timeDelta = Seconds(0) // Get ready to rerun assign power at the same time but new configuration.
          mutableDevices.find(_.uuid == u.device).foreach(_.updateState(u.consumption, u.production))
      }

      //log += EventLogItem(next)
      if (configuration.trace) println(traceEvent(next))
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
