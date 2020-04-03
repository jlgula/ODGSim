package org.opendcgrid.app.sim

import squants.energy.{Power, Watts}

object Samples {
  val internalConsumption1: Power = Watts(10)
  val internalConsumption2: Power = Watts(30)
  val internalProduction1: Power = Watts(20)
  val internalProduction2: Power = Watts(10)

  val deviceName = "testDevice0"
  val deviceUUID = 0
  val port0: Port = Port(deviceUUID, "port0")
  val port1: Port = Port(deviceUUID, "port1")
  val ports = Seq(port0, port1)
  val device0 = new Device(deviceName, deviceUUID, Seq())

  val deviceName1 = "testLoadDevice1"
  val deviceUUID1 = 1
  val port10: Port = Port(deviceUUID1, "loadPort10")
  val device1 = new Device(deviceName1, deviceUUID1, Seq(port10), initialInternalConsumption = internalConsumption1)

  val deviceUUID2 = 2
  val port20: Port = Port(deviceUUID2, "loadPort20")
  val port21: Port = Port(deviceUUID2, "loadPort21")
  val deviceName2 = "testLoadDevice2"
  val device2 = new Device(deviceName2, deviceUUID2, Seq(port20, port21), initialInternalConsumption = internalConsumption1)

  val deviceUUID3 = 3
  val deviceName3 = "testLoadDevice3"
  val device3 = new Device(deviceName3, deviceUUID3, Seq(), initialInternalConsumption = internalConsumption1, initialInternalProduction = internalProduction1)

  val deviceUUID4 = 4
  val device4 = new Device("testLoadDevice4", deviceUUID4, Seq(), initialInternalConsumption = internalConsumption2, initialInternalProduction = internalProduction1)

  // Basic source device
  val deviceUUID5 = 5
  val port50: Port = Port(deviceUUID5, "sourcePort50", Direction.Source)
  val device5 = new Device("testSourceDevice5", deviceUUID5, Seq(port50), initialInternalProduction = internalProduction1)

  // Daisy-chain repeater device
  val deviceUUID6 = 6
  val port60: Port = Port(deviceUUID6, "port60", Direction.Source)
  val port61: Port = Port(deviceUUID6, "port61", Direction.Load)
  val device6 = new Device("device6", deviceUUID6, Seq(port60, port61))

  // Two sources
  val deviceUUID7 = 7
  val port70: Port = Port(deviceUUID7, "port70", Direction.Load)
  val port71: Port = Port(deviceUUID7, "port71", Direction.Load)
  val device7 = new Device("device7", deviceUUID7, Seq(port70, port71), initialInternalConsumption = internalConsumption2)

  // Lower power source device
  val deviceUUID8 = 8
  val port80: Port = Port(deviceUUID8, "sourcePort80", Direction.Source)
  val device8 = new Device("device8", deviceUUID8, Seq(port80), initialInternalProduction = internalProduction2)

  // Source with 2 loads
  val deviceUUID9 = 9
  val port90: Port = Port(deviceUUID9, "sourcePort90", Direction.Source)
  val port91: Port = Port(deviceUUID9, "sourcePort91", Direction.Source)
  val device9 = new Device("device9", deviceUUID9, Seq(port90, port91), initialInternalProduction = internalProduction1)

  val allDevices = Seq(device0, device1, device2, device3, device4, device5, device6, device7, device8, device9)

  val grid2 = new Grid(Set(device1, device5), Map(port50 -> port10)) // Basic load and source

  // Daisy chain.
  val grid3 = new Grid(Set(device1, device5, device6), Map(port50 -> port61, port60 -> port10))

  // One load with dual sources
  val grid4 = new Grid(Set(device5, device7, device8), Map(port50 -> port70, port80 -> port71))

  // One source with 2 loads
  val grid5 = new Grid(Set(device1, device2, device9), Map(port90 -> port10, port91 -> port20))

}
