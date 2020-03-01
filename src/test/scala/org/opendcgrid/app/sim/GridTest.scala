package org.opendcgrid.app.sim

import squants.energy.{Power, Watts}
import squants.time.{Seconds, Time}
import Samples._

class GridTest extends org.scalatest.FunSuite {

  test("EmptyGrid") {
    val grid = new Grid()
    val result = grid.run(Seq())
    assert(result.isEmpty)
  }

  test("EmptyGridWithQuit") {
    val grid = new Grid()
    val event = QuitEvent()
    val result = grid.run(Seq(event))
    assert(result.size == 1)
    assert(result.head == EventLogItem(event))
  }

  test("SelfContainedDeviceWithEnoughPower") {
    val grid = new Grid(Set(device3))
    val event = QuitEvent()
    val result = grid.run(Seq(event))
    assert(result.size == 1)
    assert(result.head == EventLogItem(event))
  }

  test("SelfContainedDeviceWithoutEnoughPower") {
    val grid = new Grid(Set(device4))
    val event = QuitEvent()
    val result = grid.run(Seq(event))
    assert(result.size == 2)
    assert(result.head == EventLogItem(event))
    assert(result(1) == UnderPowerLogItem(Seconds(0), device4.deviceID, internalConsumption2, internalProduction1)
    )
  }

  test("BasicLoadAndSource") {
    val grid = new Grid(Set(loadDevice1, sourceDevice5), Map(sourcePort50 -> loadPort10))
    val event = QuitEvent()
    val result = grid.run(Seq(event))
    assert(result.size == 1)
    assert(result.head == EventLogItem(event))

  }

}
