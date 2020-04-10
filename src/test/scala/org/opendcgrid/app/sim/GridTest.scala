package org.opendcgrid.app.sim

import squants.time.Seconds
import Samples._
import squants.energy.Watts

class GridTest extends org.scalatest.funsuite.AnyFunSuite {

  test("EmptyGrid") {
    val grid = new Grid()
    val log = grid.run()
    assert(log.isEmpty)
  }

  test("EmptyGridWithTick") {
    val grid = new Grid()
    val event = TickEvent(Seconds(1))
    val log = grid.run(RunConfiguration(Seq(event)))
    //assert(log.size == 1)
    //assert(log.head == EventLogItem(event))
    assert(log.isEmpty)

  }

  test("SelfContainedDeviceWithEnoughPower") {
    val grid = new Grid(Set(device3))
    val log = grid.run()
    assert(log.isEmpty)
  }

  test("SelfContainedDeviceWithoutEnoughPower") {
    val grid = new Grid(Set(device4))
    val log = grid.run()
    assert(log.size == Parameters.tickCount)
    for (tick <- 0 until Parameters.tickCount) {
      val item = log(tick)
      val time = tick * Parameters.tickInterval
      assert(item == UnderPowerLogItem(time, device4.deviceID, internalConsumption30, Watts(0)))
    }
  }

  test("BasicSourceAndLoad") {
    val log = grid2.run()
    //val log = grid2.run(RunConfiguration(Nil, Some("BasicSourceAndLoad"), true))
    assert(log.isEmpty)
  }

  test("DaisyChain") {
    val log = grid3.run()
    //val log = grid3.run(RunConfiguration(Nil, Some("DaisyChain"), true))
    assert(log.isEmpty)
  }

  test("TwoSources") {
    val log = grid4.run()
    //val log = grid4.run(Nil, RunConfiguration(Some("TwoSources"), trace = true))
    assert(log.isEmpty)
  }

  test("TwoLoads") {
    val log = grid5.run()
    //val log = grid5.run(RunConfiguration(Nil, Some("TwoLoads"), trace = true))
    assert(log.isEmpty)
  }

  test("Assymmetric") {
    val log = grid6.run()
    //val log = grid6.run(RunConfiguration(Nil, Some("Assymmetric"), trace = true))
    assert(log.isEmpty)
  }

  test("OffDevice") {
    val log = grid7.run()
    //val log = grid7.run(RunConfiguration(Nil, Some("OffDevice"), trace = true))
    assert(log.isEmpty)
  }

  test("OffDevice delayed start") {
    val events = Seq(UpdateDeviceState(Seconds(1), device = deviceUUID11, consumption = Watts(10)), UpdateDeviceState(Seconds(2), device = deviceUUID11, consumption = Watts(0)))
    val config = RunConfiguration(events)
    //val config = RunConfiguration(events, Some("OffDevice delayed start"), trace = true)
    val log = grid7.run(config)
    assert(log.isEmpty)
  }


}
