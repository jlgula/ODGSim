package org.opendcgrid.app.sim

import squants.time.{Seconds, Time}
import squants.energy.{Energy, Power, WattHours, Watts}

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

case class Battery(capacity: Energy, chargeRate: Power, dischargeRate: Power, initialCharge: Energy = WattHours(0))


sealed abstract class Event(val time: Time)

// Tick events are used to mark the passage of time, for example, charging a battery.
case class TickEvent(t: Time = Seconds(0)) extends Event(t)

// Used to turn on or off devices, either sources or loads.
case class UpdateDeviceState(t: Time = Seconds(0), device: Int, consumption: Power = Watts(0), production: Power = Watts(0)) extends Event(t)


sealed abstract class LogItem(val time: Time)

case class EventLogItem(event: Event) extends LogItem(event.time)

case class UnderPowerLogItem(t: Time, device: String, expected: Power, assigned: Power) extends LogItem(t)

sealed abstract class PowerMessage(val port: Port, val power: Power)

case class PowerRequest(pt: Port, pwr: Power) extends PowerMessage(pt, pwr)

case class PowerGrant(pt: Port, pwr: Power) extends PowerMessage(pt, pwr)

case class RunConfiguration(toDo: Seq[Event] = Nil, name: Option[String] = None, trace: Boolean = false)


