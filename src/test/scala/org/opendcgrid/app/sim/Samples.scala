package org.opendcgrid.app.sim

import squants.energy.Watts

object Samples {
  val internalConsumption1 = Watts(10)
  val internalConsumption2 = Watts(30)
  val internalProduction1 = Watts(20)

  val deviceName = "testDevice0"
  val deviceUUID = 0
  val port0 = Port(deviceUUID, "port0")
  val port1 = Port(deviceUUID, "port1")
  val ports = Seq(port0, port1)
  val emptyDevice = new Device(deviceName, deviceUUID, Seq())

  val deviceName1 = "testLoadDevice1"
  val deviceUUID1 = 1
  val loadPort10 = Port(deviceUUID1, "loadPort10")
  val loadDevice1 = new Device(deviceName1, deviceUUID1, Seq(loadPort10), initialInternalConsumption = internalConsumption1)

  val deviceUUID2 = 2
  val loadPort20 = Port(deviceUUID2, "loadPort20")
  val loadPort21 = Port(deviceUUID2, "loadPort21")
  val deviceName2 = "testLoadDevice2"
  val loadDevice2 = new Device(deviceName2, deviceUUID2, Seq(loadPort20, loadPort21), initialInternalConsumption = internalConsumption1)


  val deviceUUID3 = 3
  val deviceName3 = "testLoadDevice3"
  val device3 = new Device(deviceName3, deviceUUID3, Seq(), initialInternalConsumption = internalConsumption1, initialInternalProduction = internalProduction1)
  val device4 = new Device("testLoadDevice4", 4, Seq(), initialInternalConsumption = internalConsumption2, initialInternalProduction = internalProduction1)

  val deviceUUID5 = 5
  val sourcePort50 = Port(deviceUUID5, "sourcePort50", Direction.Source)
  val sourceDevice5 = new Device("testSourceDevice5", deviceUUID5, Seq(sourcePort50), initialInternalProduction = internalProduction1)

  val allDevices = Seq(emptyDevice, loadDevice1, loadDevice2)
}
