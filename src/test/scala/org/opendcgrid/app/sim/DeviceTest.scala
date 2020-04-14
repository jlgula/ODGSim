package org.opendcgrid.app.sim

import squants.energy.Watts
import Samples._
//import org.scalactic.Fail
import squants.time.Seconds

class DeviceTest extends org.scalatest.funsuite.AnyFunSuite {

  test("Device") {
    val device = new BasicDevice(deviceName, deviceUUID, ports)
    assert(device.deviceID == deviceName)
  }


  test("initializePowerCycle") {
    for (device <- Seq(device9.buildMutableDevice())) {
      //device.reset()
      device.initializePowerCycle(Seconds(0), Map[Port, Port]())
      assert(!device.hasMessagesToProcess)
      assertResult(Watts(0))(device.assignedInternalConsumption)
      //assertResult(device.internalConsumption)(device.totalPowerDemand)
      //assertResult(device.internalProduction)(device.totalPowerAvailable)
      //assert(device.requestsPending.isEmpty)
      //assert(device.grantsPending.isEmpty)
    }
  }

  test("assignPower") {
    for (device <- allDevices.map(_.buildMutableDevice())) {
      //device.reset()
      device.initializePowerCycle(Seconds(0), Map[Port, Port]())
      val _ = device.assignPower(Parameters.tickInterval)
    }
  }
  /*
    test("minimumGrantFromInternalProduction") {
      //for (device <- allDevices) {
      for (device <- allDevices.map(_.buildMutableDevice())) {
        // device.reset()
        //println(device.deviceID)
        device.updatePowerState(Map[Port, Port]())
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

    test("grantLessThanRequest") {
      //for (device <- allDevices) {
      val device = device8.buildMutableDevice()
      device.updatePowerState(Map(port80 -> port10))
      device.postMessage(PowerRequest(port80, internalConsumption20))
      val grants = device.assignPower()
      assert(grants.size == 1)
      val grant = grants.head
      grant match {
        case g: PowerGrant => assertResult(device.internalProduction)(g.pwr)
        case _ => Fail(s"Unexpected response: $grant")
      }
    }
   */
}
