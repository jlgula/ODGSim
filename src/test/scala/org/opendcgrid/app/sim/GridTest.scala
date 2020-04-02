package org.opendcgrid.app.sim

import squants.time.{Seconds}
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

  test("BasicSourceAndLoad") {
    val event = QuitEvent()
    val result = grid2.run(Seq(event))
  }


  test("DaisyChain") {
    val quitEvent = QuitEvent()
    val tickEvent = TickEvent()
    //val result = grid3.run(Seq(event))
    //val result = grid3.run(Seq(tickEvent, QuitEvent(Seconds(1))), RunConfiguration(Some("DaisyChain"), true))
    val result = grid3.run(Seq(tickEvent, QuitEvent(Seconds(1))))
  }

}
