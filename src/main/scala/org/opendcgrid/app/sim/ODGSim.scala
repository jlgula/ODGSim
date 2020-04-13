package org.opendcgrid.app.sim

import squants.time.{Seconds, Time}
import squants.energy.{Energy, KilowattHours, Power, WattHours, Watts}
import squants.market.{Price, USD}

object Parameters {
  val maxEvents = 1000 // Maximum number of events processed by Grid.run
  val maxPowerIterations = 1000 // Maximum number of passes through the assign power loop.
  val maxEnergyIterations = 1000 // Maximum number of passes through energy assignment.
  val tickCount = 10 // Number of tick events to process
  val tickInterval: Time = Seconds(1)
  val powerPrice: Price[Energy] = USD(1) / KilowattHours(1)
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

case class Battery(capacity: Energy, chargeRate: Power, dischargeRate: Power, initialCharge: Energy = WattHours(0))

object NullBattery extends Battery(WattHours(0), Watts(0), Watts(0))

// Events processed by the event loop.
sealed abstract class Event(val time: Time)

// Tick events are used to mark the passage of time, for example, charging a battery.
case class TickEvent(t: Time = Seconds(0)) extends Event(t)

// Used to turn on or off devices, either sources or loads.
case class UpdateDeviceState(t: Time = Seconds(0), device: Int, consumption: Power = Watts(0), production: Power = Watts(0)) extends Event(t)

// Types of LogItem
sealed abstract class LogItemType(val name: String)

object LogItemType {

  case object Condition extends LogItemType("Condition")

  case object Event extends LogItemType("Event")

  case object Device extends LogItemType("Device")

  case object Port extends LogItemType("Port")

  case object Message extends LogItemType("Message")

  case object Configuration extends LogItemType("Configuration")

}


// Things that go into the log.
sealed abstract class LogItem(val time: Time, val itemType: LogItemType) {
  def detail: String

  override def toString: String = s"$time ${itemType.name} $detail"
}

object LogItem {

  case class Configuration(t: Time, name: String) extends LogItem(t, LogItemType.Configuration) {
    def detail: String = name
  }

  case class EventItem(event: Event) extends LogItem(event.time, LogItemType.Event) {
    def detail: String = event.toString
  }

  case class UnderPower(t: Time, device: String, expected: Power, assigned: Power) extends LogItem(t, LogItemType.Condition) {
    def detail = s"under-power expected: $expected assigned: $assigned"
  }

  case class SufficientPower(t: Time, device: String, expected: Power, assigned: Power) extends LogItem(t, LogItemType.Condition) {
    def detail = s"sufficient-power expected: $expected assigned: $assigned"
  }

  case class Device(t: Time, name: String, uuid: Int, consumption: Power, production: Power, charge: Energy, on: Boolean) extends LogItem(t, LogItemType.Device) {
    def detail = s"$name:$uuid consumption: $consumption production: $production charge: $charge on: $on"
  }

  case class Port(t: Time, device: String, port: String, power: Power) extends LogItem(t, LogItemType.Device) {
    def detail = s"device: $device port: $port power: $power"
  }

  case class Message(t: Time, source: Int, target: Int, message: PowerMessage) extends LogItem(t, LogItemType.Message) {
    def detail = s"source: $source target: $target message: $message"
  }

}

// What to log or trace.
sealed abstract class ReportSelection

object ReportSelection {

  case object UnderPower extends ReportSelection // Log cycle when device did not have enough power to operate
  case object SufficientPower extends ReportSelection // Log cycle when device did have enough power to operate
  case object DeviceStatus extends ReportSelection // General device status
  case object PortStatus extends ReportSelection // General port status
  case object ConfigurationName extends ReportSelection // Name of run configuration
  case object TickEvent extends ReportSelection // Timer ticks
  case object UpdateDeviceEvent extends ReportSelection // Events that change the state of the device
  case object PowerMessage extends ReportSelection // Messages between devices

  val all: Set[ReportSelection] = Set(UnderPower, SufficientPower, DeviceStatus, PortStatus, ConfigurationName, TickEvent, UpdateDeviceEvent, PowerMessage)

}

// Simulated network messages.
sealed abstract class PowerMessage(val port: Port, val power: Power)

case class PowerRequest(pt: Port, pwr: Power) extends PowerMessage(pt, pwr)

case class PowerGrant(pt: Port, pwr: Power) extends PowerMessage(pt, pwr)

case class PowerPrice(pt: Port, pwr: Power, price: Price[Energy]) extends PowerMessage(pt, pwr)


// Structure defines characteristics of a simulation run.
case class RunConfiguration(
                             toDo: Seq[Event] = Nil,
                             name: Option[String] = None,
                             trace: Set[ReportSelection] = Set(),
                             log: Set[ReportSelection] = Set(ReportSelection.UnderPower),
                             tickCount: Int = Parameters.tickCount,
                             tickInterval: Time = Parameters.tickInterval)
