package org.opendcgrid.app.sim

import squants.time.Seconds
import Samples._

class GridTest extends org.scalatest.FunSuite {

  test("EmptyGrid") {
    val grid = new Grid()
    val log = grid.run()
    assert(log.isEmpty)
  }

  test("EmptyGridWithTick") {
    val grid = new Grid()
    val event = TickEvent(Seconds(1))
    val log = grid.run(RunConfiguration(Seq(event)))
    assert(log.size == 1)
    assert(log.head == EventLogItem(event))
  }

  test("SelfContainedDeviceWithEnoughPower") {
    val grid = new Grid(Set(device3))
    val log = grid.run()
    assert(log.isEmpty)
  }

  test("SelfContainedDeviceWithoutEnoughPower") {
    val grid = new Grid(Set(device4))
    val log = grid.run()
    assert(log.size == 1)
    assert(log.head == UnderPowerLogItem(Seconds(0), device4.deviceID, internalConsumption2, internalProduction1))
  }

  test("BasicSourceAndLoad") {
    val log = grid2.run()
    assert(log.isEmpty)
  }

  test("DaisyChain") {
    val log = grid3.run()
    //val log = grid3.run(Nil, RunConfiguration(Some("DaisyChain"), true))
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

}
