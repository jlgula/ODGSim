package org.opendcgrid.app.sim

import squants.time.{Seconds, Time}
import squants.energy.{Energy, Power, WattHours, Watts}

object Parameters {
  val maxEvents = 1000 // Maximum number of events processed by Grid.run
  val maxPowerIterations = 1000 // Maximum number of passes through the assign power loop.
  val maxEnergyIterations = 1000 // Maximum number of passes through energy assignment.
  val tickCount = 10 // Number of tick events to process
  val tickInterval: Time = Seconds(1)

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


// Things that go into the log.
sealed abstract class LogItem(val time: Time)

case class EventLogItem(event: Event) extends LogItem(event.time)

case class UnderPowerLogItem(t: Time, device: String, expected: Power, assigned: Power) extends LogItem(t)


// Simulated network messages.
sealed abstract class PowerMessage(val port: Port, val power: Power)

case class PowerRequest(pt: Port, pwr: Power) extends PowerMessage(pt, pwr)

case class PowerGrant(pt: Port, pwr: Power) extends PowerMessage(pt, pwr)


// Structure defines characteristics of a simulation run.
case class RunConfiguration(toDo: Seq[Event] = Nil, name: Option[String] = None, trace: Boolean = false, tickCount: Int = Parameters.tickCount, tickInterval: Time = Parameters.tickInterval)
