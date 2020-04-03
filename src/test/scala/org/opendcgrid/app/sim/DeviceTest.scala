package org.opendcgrid.app.sim

import squants.energy.Watts
import Samples._

class DeviceTest extends org.scalatest.FunSuite {

  test("Device") {
    val device = new Device(deviceName, deviceUUID, ports)
    assert(device.deviceID == deviceName)
  }


  test("initializePowerCycle") {
    for (device <- Seq(device9)) {
      device.reset()
      device.initializePowerCycle(Map[Port, Port]())
      assert(device.pendingMessages.isEmpty)
      assertResult(Watts(0))(device.assignedInternalConsumption)
      assertResult(device.internalConsumption)(device.totalPowerDemand)
      assertResult(device.internalProduction)(device.totalPowerAvailable)
      assert(device.requestsPending.isEmpty)
      assert(device.grantsPending.isEmpty)
    }
  }

  test("assignPower") {
    for (device <- allDevices) {
      device.reset()
      device.initializePowerCycle(Map[Port, Port]())
      val _ = device.assignPower()
    }
  }

  test("minimumGrantFromInternalProduction") {
    //for (device <- allDevices) {
    for (device <- allDevices) {
      device.reset()
      //println(device.deviceID)
      device.initializePowerCycle(Map[Port, Port]())
      if (device.totalPowerAvailable >= Watts(1)) {
        if (device.allSourcePorts.nonEmpty) {
          val port = device.allSourcePorts.head
          device.pendingMessages += PowerRequest(port, Watts(1))
          val grants = device.assignPower()
          assert(grants.nonEmpty)
        }
      }
    }

  }
}
