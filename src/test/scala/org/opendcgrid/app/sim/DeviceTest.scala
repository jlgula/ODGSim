package org.opendcgrid.app.sim

import squants.energy.Watts
import Samples._

class DeviceTest extends org.scalatest.FunSuite {

  test("Device") {
    val device = new Device(deviceName, deviceUUID, ports)
    assert(device.deviceID == deviceName)
  }


  test("initializePowerCycle") {
    for (device <- allDevices) {
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
    val result1 = device0.assignPower()
    assert(result1.isEmpty)
    /*
        // Test simple load
        val timeDelta = Seconds(10)
        val loadData = loadDevice1.createData()
        val result2 = loadDevice1.assignPower(loadData)
        assertResult(1)(result2.size)
        //assertResult(PowerRequest(loadData.internalConsumption * timeDelta))(result2(loadPort10))

        // Test dual load ports
        val loadData2 = loadDevice2.createData()
        val result3 = loadDevice2.assignPower(loadData2)
        //assertResult(2)(result3.size)
        //val expectedPortPower = PowerRequest(loadData2.internalConsumption * timeDelta / 2)
        //assertResult(expectedPortPower)(result3(loadPort20))
        //assertResult(expectedPortPower)(result3(loadPort21))
    */

  }
}
