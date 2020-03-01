package org.opendcgrid.app.sim

import squants.energy.{Energy, Power, WattHours, Watts}
import squants.time.{Seconds, Time}
import Samples._

class DeviceTest extends org.scalatest.FunSuite {

  test("Device") {
    val device = new Device(deviceName, deviceUUID, ports)
    assert(device.deviceID == deviceName)
  }

  test("createData") {
    for (device <- allDevices) {
      val data = device.createData()
      assertResult(device.internalProduction)(data.internalProduction)
      assertResult(device.internalConsumption)(data.internalConsumption)
      assert(data.pendingMessages.isEmpty)
      assertResult(device.ports.size)(data.portDirections.size)
      assertResult(device.ports.size)(data.powerFlow.size)
    }
  }

  test("initializePowerCycle") {
    for (device <- allDevices) {
      val data = emptyDevice.createData()
      device.initializePowerCycle(data, Map[Port, Port]())
      assert(data.pendingMessages.isEmpty)
      assertResult(Watts(0))(data.assignedInternalConsumption)
      assertResult(data.internalConsumption)(data.totalPowerDemand)
      assertResult(device.internalProduction)(data.totalPowerAvailable)
      assert(data.requestsPending.isEmpty)
      assert(data.grantsPending.isEmpty)
    }
  }

  test("assignPower") {
    val emptyData = emptyDevice.createData()
    val result1 = emptyDevice.assignPower(emptyData)
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
