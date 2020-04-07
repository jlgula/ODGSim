package org.opendcgrid.app.sim

import squants.time.{Seconds, Time}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

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

      if (configuration.trace) {
        for (device <- mutableDevices) {
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

    def traceMessage(sourceDevice: Device, targetDevice: Device, message: PowerMessage): String = {
      s"$timeOffset source: ${sourceDevice.deviceID} target: ${targetDevice.deviceID} message: $message"
    }

    def traceEvent(event: Event): String = {
      s"$timeOffset event: $event"
    }

    def traceDevicePort(device: MutableDevice, port: Port): String = {
      s"$timeOffset device: ${port.uuid} port: ${port.name}, power: ${device.portPower(port)}"
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
        case u: UpdateDeviceState => mutableDevices.find(_.uuid == u.device).foreach(_.updateState(u.consumption, u.production))
      }

      //log += EventLogItem(next)
      if (configuration.trace) println(traceEvent(next))
      assignPower()

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
