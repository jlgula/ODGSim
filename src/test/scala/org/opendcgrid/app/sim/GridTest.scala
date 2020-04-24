package org.opendcgrid.app.sim

import squants.time.{Hours, Seconds}
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
      assert(item == LogItem.UnderPower(time, device4.deviceID, internalConsumption30, Watts(0)))
    }
  }

  test("BasicSourceAndLoad") {
    val config: RunConfiguration = RunConfiguration()
    //val config: RunConfiguration = RunConfiguration(trace = ReportSelection.all)
    val log = grid2.run(config)
    assert(log.isEmpty)
  }

  test("DaisyChain") {
    val config: RunConfiguration = RunConfiguration()
    //val config: RunConfiguration = RunConfiguration(trace = ReportSelection.all)
    val log = grid3.run(config)
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
    val config: RunConfiguration = RunConfiguration()
    //val config: RunConfiguration = RunConfiguration(name = Some("Assymmetric"), tickInterval = Hours(1), trace = ReportSelection.all, log = Set())
    val log = grid6.run(config)
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

  test("DaisyChainWithLongInterval") {
    val config: RunConfiguration = RunConfiguration(tickInterval = Hours(1))
    val log = grid3.run(config)
    //val log = grid3.run(RunConfiguration(Nil, Some("DaisyChain"), true))
    assert(log.isEmpty)
  }

  test("SelfContainedDeviceWithBattery") {
    val config: RunConfiguration = RunConfiguration(tickInterval = Hours(1), log = Set())
    //val config: RunConfiguration = RunConfiguration(tickInterval = Hours(1), trace = ReportSelection.all, log = Set())
    val log = grid8.run(config)
    assert(log.isEmpty)
  }

  test("DaisyChainWithBattery") {
    val config: RunConfiguration = RunConfiguration(tickInterval = Hours(0.5), log = Set(ReportSelection.DeviceStatus))
    //val config: RunConfiguration = RunConfiguration(tickInterval = Hours(0.5), trace = ReportSelection.all, log = Set(ReportSelection.DeviceStatus))
    val log = grid9.run(config)
    val device7OnStatus = log.collect { case LogItem.Device(_, _, 7, _, _, _, on) => on }
    assert(device7OnStatus.tail.contains(true))
    //device7OnStatus.tail.foreach(println(_))
  }

  test("TwoLoadsOneRefusesPrice") {
    val config: RunConfiguration = RunConfiguration(log = Set(ReportSelection.DeviceStatus))
    //val config: RunConfiguration = RunConfiguration(trace = ReportSelection.all, log = Set(ReportSelection.DeviceStatus))
    val log = grid10.run(config)
    val device1Status = log.collect { case LogItem.Device(_, _, 1, _, _, _, on) => on }
    val device14Status = log.collect { case LogItem.Device(_, _, 14, _, _, _, on) => on }
    assert(device1Status.forall(result => result)) // Device 1 should be always on.
    assert(!device14Status.tail.exists(result => result)) // Device 14 stays off after initialization
  }

  test("TwoLoadsOneOutBidsTheOther") {
    val config: RunConfiguration = RunConfiguration(log = Set(ReportSelection.DeviceStatus))
    //val config: RunConfiguration = RunConfiguration(trace = ReportSelection.all, log = Set(ReportSelection.DeviceStatus))
    val log = grid11.run(config)
    val device1Status = log.collect { case LogItem.Device(_, _, 1, _, _, _, on) => on }
    val device15Status = log.collect { case LogItem.Device(_, _, 15, _, _, _, on) => on }
    assert(device15Status.forall(result => result)) // Device 1 should be always on.
    assert(!device1Status.tail.exists(result => result)) // Device 14 stays off after initialization
  }

  test("DaisyChainWithLowPrice") {
    val config: RunConfiguration = RunConfiguration()
    //val config: RunConfiguration = RunConfiguration(trace = ReportSelection.all, name = Some("DaisyChainWithLowPrice"))
    val log = grid12.run(config)
    assert(log.isEmpty)
  }

  test("CombinerMergesPrices") {
    val config: RunConfiguration = RunConfiguration()
    //val config: RunConfiguration = RunConfiguration(trace = ReportSelection.all, name = Some("CombinerMergesPrices"))
    val log = grid13.run(config)
    assert(log.isEmpty)
  }


}
