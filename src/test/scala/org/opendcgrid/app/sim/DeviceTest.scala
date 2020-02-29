package org.opendcgrid.app.sim

import squants.energy.{Energy, Power, WattHours, Watts}
import squants.time.{Seconds, Time}

class DeviceTest extends org.scalatest.FunSuite {
  val deviceName = "testDevice0"
  val deviceUUID = 0
  val port0 = Port(deviceUUID, "port0")
  val port1 = Port(deviceUUID, "port1")
  val ports = Seq(port0, port1)
  val emptyDevice = new Device(deviceName, deviceUUID, Seq())

  val deviceName1 = "testLoadDevice1"
  val deviceUUID1 = 1
  val loadPort10 = Port(deviceUUID1, "loadPort10")
  val internalConsumption1 = Watts(10) // 10 Watts of power
  val loadDevice1 = new Device(deviceName1, deviceUUID1, Seq(loadPort10), internalConsumption = internalConsumption1)

  val deviceUUID2 = 2
  val loadPort20 = Port(deviceUUID2, "loadPort20")
  val loadPort21 = Port(deviceUUID2, "loadPort21")
  val deviceName2 = "testLoadDevice2"
  val loadDevice2 = new Device(deviceName2, deviceUUID2, Seq(loadPort20, loadPort21), internalConsumption = internalConsumption1)


  test("Device") {
    val device = new Device(deviceName, deviceUUID, ports)
    assert(device.deviceID == deviceName)
  }

  test("createData") {
    val emptyData = emptyDevice.createData()
    assertResult(Watts(0))(emptyData.internalProduction)
    assertResult(Watts(0))(emptyData.internalConsumption)
    //assertResult(WattHours(0))(emptyData.remainingInternalEnergy)
    assert(emptyData.pendingMessages.isEmpty)
    assert(emptyData.portDirections.isEmpty)
    assert(emptyData.powerFlow.isEmpty)

    val loadInitial1 = loadDevice1.createData()
    assertResult(Watts(0))(loadInitial1.internalProduction)
    assertResult(internalConsumption1)(loadInitial1.internalConsumption)
    //assertResult(WattHours(0))(loadInitial1.remainingInternalEnergy)
    assert(loadInitial1.pendingMessages.isEmpty)
    assertResult(1)(loadInitial1.portDirections.size)
    assertResult(Direction.Load)(loadInitial1.portDirections(loadPort10))
    assertResult(1)(loadInitial1.powerFlow.size)
    assertResult(Watts(0))(loadInitial1.powerFlow(loadPort10))
    /*
        val timeDelta = Seconds(10)
        val loadInitial2 = loadDevice1.createData(timeDelta)
        assertResult(Watts(0))(loadInitial2.internalProduction)
        assertResult(internalConsumption1)(loadInitial2.internalConsumption)
        //assertResult(WattHours(0))(loadInitial2.remainingInternalEnergy)
        assertResult(1)(loadInitial2.pendingMessages.size)
        //assertResult(PowerRequest(timeDelta * internalConsumption1))(loadInitial2.pendingMessages.dequeue())
        assertResult(1)(loadInitial2.portDirections.size)
        assertResult(Direction.Load)(loadInitial2.portDirections(loadPort10))
        assertResult(1)(loadInitial2.powerFlow.size)
        assertResult(Watts(0))(loadInitial2.powerFlow(loadPort10))

        val loadInitial21 = loadDevice2.createData(timeDelta)
        assertResult(Watts(0))(loadInitial21.internalProduction)
        assertResult(internalConsumption1)(loadInitial21.internalConsumption)
        //assertResult(WattHours(0))(loadInitial21.remainingInternalEnergy)
        assertResult(1)(loadInitial21.pendingMessages.size)
        //assertResult(PowerRequest(timeDelta * internalConsumption1))(loadInitial21.pendingMessages.dequeue())
        assertResult(2)(loadInitial21.portDirections.size)
        assertResult(Direction.Load)(loadInitial21.portDirections(loadPort20))
        assertResult(Direction.Load)(loadInitial21.portDirections(loadPort21))
        assertResult(2)(loadInitial21.powerFlow.size)
        assertResult(Watts(0))(loadInitial21.powerFlow(loadPort20))
        assertResult(Watts(0))(loadInitial21.powerFlow(loadPort21))
      */
  }

  test("assignPower") {
    val emptyData = emptyDevice.createData()
    val result1 = emptyDevice.assignPower(emptyData)
    assert(result1.isEmpty)

    // Test simple load
    val timeDelta = Seconds(10)
    val loadData = loadDevice1.createData(timeDelta)
    val result2 = loadDevice1.assignPower(loadData)
    assertResult(1)(result2.size)
    //assertResult(PowerRequest(loadData.internalConsumption * timeDelta))(result2(loadPort10))

    // Test dual load ports
    val loadData2 = loadDevice2.createData(timeDelta)
    val result3 = loadDevice2.assignPower(loadData2)
    //assertResult(2)(result3.size)
    //val expectedPortPower = PowerRequest(loadData2.internalConsumption * timeDelta / 2)
    //assertResult(expectedPortPower)(result3(loadPort20))
    //assertResult(expectedPortPower)(result3(loadPort21))

  }

}
