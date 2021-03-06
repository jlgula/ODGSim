package org.opendcgrid.app.sim

import squants.energy.{Power, WattHours, Watts}

object Samples {
  val internalConsumption10: Power = Watts(10)
  val internalConsumption20: Power = Watts(20)
  val internalConsumption30: Power = Watts(30)
  val internalProduction20: Power = Watts(20)
  val internalProduction10: Power = Watts(10)

  val battery01: Battery = Battery(capacity = WattHours(10), chargeRate = Watts(10), dischargeRate = Watts(20))

  val deviceName = "testDevice0"
  val deviceUUID = 0
  val port0: Port = Port(deviceUUID, "port0")
  val port1: Port = Port(deviceUUID, "port1")
  val ports = Seq(port0, port1)
  val device0 = new BasicDevice(deviceName, deviceUUID, Seq())

  // Basic load device with a 10W load.
  val deviceName1 = "testLoadDevice1"
  val deviceUUID1 = 1
  val port10: Port = Port(deviceUUID1, "loadPort10")
  val device1 = new BasicDevice(deviceName1, deviceUUID1, Seq(port10), internalConsumption = internalConsumption10)

  val deviceUUID2 = 2
  val port20: Port = Port(deviceUUID2, "loadPort20")
  val port21: Port = Port(deviceUUID2, "loadPort21")
  val deviceName2 = "testLoadDevice2"
  val device2 = new BasicDevice(deviceName2, deviceUUID2, Seq(port20, port21), internalConsumption = internalConsumption10)

  val deviceUUID3 = 3
  val deviceName3 = "testLoadDevice3"
  val device3 = new BasicDevice(deviceName3, deviceUUID3, Seq(), internalConsumption = internalConsumption10, internalProduction = internalProduction20)

  val deviceUUID4 = 4
  val device4 = new BasicDevice("testLoadDevice4", deviceUUID4, Seq(), internalConsumption = internalConsumption30, internalProduction = internalProduction20)

  // Basic source device
  val deviceUUID5 = 5
  val port50: Port = Port(deviceUUID5, "sourcePort50", Direction.Source)
  val device5 = new BasicDevice("testSourceDevice5", deviceUUID5, Seq(port50), internalProduction = internalProduction20)

  // Daisy-chain repeater device
  val deviceUUID6 = 6
  val port60: Port = Port(deviceUUID6, "port60", Direction.Source)
  val port61: Port = Port(deviceUUID6, "port61", Direction.Load)
  val device6 = new BasicDevice("device6", deviceUUID6, Seq(port60, port61))

  // Two load ports, 30W
  val deviceUUID7 = 7
  val port70: Port = Port(deviceUUID7, "port70", Direction.Load)
  val port71: Port = Port(deviceUUID7, "port71", Direction.Load)
  val device7 = new BasicDevice("device7", deviceUUID7, Seq(port70, port71), internalConsumption = internalConsumption30)

  // Lower power source device
  val deviceUUID8 = 8
  val port80: Port = Port(deviceUUID8, "sourcePort80", Direction.Source)
  val port81: Port = Port(deviceUUID8, "sourcePort81", Direction.Source)
  val device8 = new BasicDevice("device8", deviceUUID8, Seq(port80, port81), internalProduction = internalProduction10)

  // Source with 2 source ports to potentially support 2 loads.
  val deviceUUID9 = 9
  val port90: Port = Port(deviceUUID9, "sourcePort90", Direction.Source)
  val port91: Port = Port(deviceUUID9, "sourcePort91", Direction.Source)
  val device9 = new BasicDevice("device9", deviceUUID9, Seq(port90, port91), internalProduction = internalProduction20)

  // Source combiner
  val deviceUUID10 = 10
  val port100: Port = Port(deviceUUID10, "port100", Direction.Load)
  val port101: Port = Port(deviceUUID10, "port101", Direction.Load)
  val port102: Port = Port(deviceUUID10, "port102", Direction.Source)
  val device10 = new BasicDevice("device10", deviceUUID10, Seq(port100, port101, port102))

  // Off device
  val deviceUUID11 = 11
  val port110: Port = Port(deviceUUID11, "port110", Direction.Load)
  val device11 = new BasicDevice("device11", deviceUUID11, Seq(port110))

  // Self-contained device with production, consumption and battery.
  val deviceUUID12 = 12
  val device12 = new BasicDevice("device12", deviceUUID12, internalConsumption = Watts(20), internalProduction = Watts(10), battery = battery01)

  // Pass through device with a battery.
  val deviceUUID13 = 13
  val port130: Port = Port(deviceUUID13, "port130", Direction.Load)
  val port131: Port = Port(deviceUUID13, "port131", Direction.Source)
  val device13 = new BasicDevice("device13", deviceUUID13, ports = Seq(port130, port131), battery = battery01)

  // Load with lower than normal price it's willing to pay.
  val deviceUUID14 = 14
  val port140: Port = Port(deviceUUID14, "port140", Direction.Load)
  val device14 = new BasicDevice("device14", deviceUUID14, ports = Seq(port140), internalConsumption = internalConsumption10, initialPowerPrice = Parameters.powerPrice / 2)

  // Load with higher than normal price it's willing to pay.
  val deviceUUID15 = 15
  val port150: Port = Port(deviceUUID15, "port150", Direction.Load)
  val device15 = new BasicDevice("device15", deviceUUID15, ports = Seq(port150), internalConsumption = internalConsumption10, initialPowerPrice = Parameters.powerPrice * 2)

  // Source with low price.
  val deviceUUID16 = 16
  val port160: Port = Port(deviceUUID16, "port160", Direction.Source)
  val device16 = new BasicDevice("device16", deviceUUID16, Seq(port160), internalProduction = internalProduction20, initialPowerPrice = Parameters.powerPrice / 10)

  // Load with lower than normal price it's willing to pay and high power required.
  val deviceUUID17 = 17
  val port170: Port = Port(deviceUUID17, "port170", Direction.Load)
  val device17 = new BasicDevice("device17", deviceUUID17, ports = Seq(port170), internalConsumption = internalConsumption30, initialPowerPrice = Parameters.powerPrice / 2)

  val allDevices = Seq(device0, device1, device2, device3, device4, device5, device6, device7, device8, device9, device10, device11, device12, device13, device14, device15, device16, device17)

  val grid2 = new Grid(Set(device1, device5), Map(port50 -> port10)) // Basic load and source

  // Daisy chain.
  val grid3 = new Grid(Set(device1, device5, device6), Map(port50 -> port61, port60 -> port10))

  // One load with dual sources
  val grid4 = new Grid(Set(device5, device7, device8), Map(port50 -> port70, port80 -> port71))

  // One source with 2 loads
  val grid5 = new Grid(Set(device1, device2, device9), Map(port90 -> port10, port91 -> port20))

  // Assymetric grid with 2 sources, one combiner
  val grid6 = new Grid(Set(device7, device8, device9, device10), Map(port91 -> port71, port90 -> port101, port80 -> port100, port102 -> port70))

  // Simple source-load grid with load initially off.
  val grid7 = new Grid(Set(device11, device5), Map(port50 -> port110))

  // Minimal self-contained device with battery.
  val grid8 = new Grid(Set(device12))

  // Daisy chain with 10W source, with battery and 30W load.
  val grid9 = new Grid(Set(device13, device7, device8), Map(port80 -> port130, port131 -> port70))

  // One source with 2 loads, one of which is prioritized off by price.
  val grid10 = new Grid(Set(device1, device9, device14), Map(port90 -> port10, port91 -> port140))

  // One source with 2 loads, one of which is willing to pay more so it should get priority.
  val grid11 = new Grid(Set(device1, device8, device15), Map(port80 -> port10, port81 -> port150))

  // Daisy chain with low sell price and low buy price.
  // This verifies that the intermediate node is passing along the sell price.
  val grid12 = new Grid(Set(device16, device14, device6), Map(port160 -> port61, port60 -> port140))

  // Combiner with low aggregate sell price and low buy price.
  // This verifies that the intermediate node is passing along the sell price merged from multiple sources.
  val grid13 = new Grid(Set(device16, device8, device17, device10), Map(port160 -> port100, port80 -> port101, port102 -> port170))

}
