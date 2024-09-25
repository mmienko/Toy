object CircuitsTests extends weaver.FunSuite {

  test("Adder") {
    import Notation.`8-Bit Words`.Ints.*
    expect.same(Circuits.Adder(Zero, Zero), Zero) and
      expect.same(Circuits.Adder(Zero, One), One) and
      expect.same(Circuits.Adder(One, Zero), One) and
      expect.same(Circuits.Adder(One, One), Two) and
      expect.same(
        Circuits.Adder(
          (0, 0, 0, 1, 0, 1, 1, 1), // 23
          (0, 0, 1, 1, 0, 0, 0, 1)  // 49
        ),
        (0, 1, 0, 0, 1, 0, 0, 0) // 72
      )
  }

  test("Mux") {
    expect.same(Circuits.Mux.`3-way selection`((0, 0, 0))((1, 0, 1)), 0) and
      expect.same(Circuits.Mux.`3-way selection`((1, 0, 0))((1, 0, 1)), 1) and
      expect.same(Circuits.Mux.`3-way selection`((0, 1, 0))((1, 0, 1)), 0) and
      expect.same(Circuits.Mux.`3-way selection`((0, 0, 1))((1, 0, 1)), 1) and
      expect.same(Circuits.Mux.`3-way selection`((0, 1, 0))((0, 1, 0)), 1) and
      expect.same(Circuits.Mux.`2-way selection`((0, 0))((1, 0)), 0) and
      expect.same(Circuits.Mux.`2-way selection`((1, 0))((1, 0)), 1) and
      expect.same(Circuits.Mux.`2-way selection`((0, 1))((1, 0)), 0) and
      expect.same(Circuits.Mux.`2-way selection`((0, 1))((0, 1)), 1)
  }

  test("Decoder") {
    expect.same(Circuits.Decoder.`3-bit`((0, 0, 0)), (1, 0, 0, 0, 0, 0, 0, 0)) and
      expect.same(Circuits.Decoder.`3-bit`((0, 0, 1)), (0, 1, 0, 0, 0, 0, 0, 0)) and
      expect.same(Circuits.Decoder.`3-bit`((0, 1, 0)), (0, 0, 1, 0, 0, 0, 0, 0)) and
      expect.same(Circuits.Decoder.`3-bit`((0, 1, 1)), (0, 0, 0, 1, 0, 0, 0, 0)) and
      expect.same(Circuits.Decoder.`3-bit`((1, 0, 0)), (0, 0, 0, 0, 1, 0, 0, 0)) and
      expect.same(Circuits.Decoder.`3-bit`((1, 0, 1)), (0, 0, 0, 0, 0, 1, 0, 0)) and
      expect.same(Circuits.Decoder.`3-bit`((1, 1, 0)), (0, 0, 0, 0, 0, 0, 1, 0)) and
      expect.same(Circuits.Decoder.`3-bit`((1, 1, 1)), (0, 0, 0, 0, 0, 0, 0, 1))
  }

  test("`4-bit 2-way BUS Mux`") {
    val bus1 = (1, 0, 1, 0)
    val bus2 = (0, 0, 0, 1)
    expect.same(Circuits.`4-bit 2-way BUS Mux`(0, 0, bus1, bus2), (0, 0, 0, 0)) and
      expect.same(Circuits.`4-bit 2-way BUS Mux`(1, 0, bus1, bus2), bus1) and
      expect.same(Circuits.`4-bit 2-way BUS Mux`(0, 1, bus1, bus2), bus2)
  }

  test("`4-bit Decoder/Demux`") {
    // 0 is 1st output
    expect.same(
      Circuits.`4-bit Decoder/Demux`((0, 0, 0, 0))(1),
      (
        (1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        (1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
      )
    ) and
      expect.same(
        Circuits.`4-bit Decoder/Demux`((0, 0, 0, 0))(0),
        (
          (1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
          (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        )
      ) and // 9 is 10th output
      expect.same(
        Circuits.`4-bit Decoder/Demux`((1, 0, 0, 1))(1),
        (
          (0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0),
          (0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0)
        )
      ) and
      expect.same(
        Circuits.`4-bit Decoder/Demux`((1, 0, 0, 1))(0),
        (
          (0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0),
          (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        )
      ) and // 15 is 16th output
      expect.same(
        Circuits.`4-bit Decoder/Demux`((1, 1, 1, 1))(1),
        (
          (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1),
          (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)
        )
      ) and
      expect.same(
        Circuits.`4-bit Decoder/Demux`((1, 1, 1, 1))(0),
        (
          (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1),
          (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        )
      )
  }

  test("Incrementer") {
    import Notation.`4-Bit Words`.Ints.*
    expect.same(Circuits.Incrementer.`4-bit`(Zero), One) and
      expect.same(Circuits.Incrementer.`4-bit`(One), Two) and
      expect.same(Circuits.Incrementer.`4-bit`((0, 1, 1, 1)), (1, 0, 0, 0))
  }

  test("FlipFlop") {
    val ff = new Circuits.FlipFlop[Int]

    val o1 = ff.out
    val o2 = ff(0, 0)
    val o3 = ff(reset = 1, 0)
    val o4 = ff(reset = 0, 0)
    val o5 = ff(reset = 0, 1)
    val o6 = ff(reset = 0, 0)
    val o7 = ff(reset = 0, 1)
    val o8 = ff(reset = 1, 0)

    expect.same(o1, 0) and
      expect.same(o2, 0) and
      expect.same(o3, 0) and
      expect.same(o4, 0) and
      expect.same(o5, 1) and
      expect.same(o6, 1) and
      expect.same(o7, 1) and
      expect.same(o8, 0)
  }

  test("RegisterBit") {
    val r  = Circuits.RegisterBit[Int]()
    val o1 = r.out
    val o2 = r(input = 1, write = 0)
    val o3 = r(input = 1, write = 1)
    val o4 = r(input = 1, write = 1)
    val o5 = r(input = 0, write = 0)
    val o6 = r(input = 0, write = 1)
    val o7 = r(input = 0, write = 1)

    expect.eql(o1, 0) and
      expect.eql(o2, 0) and
      expect.eql(o3, 1) and
      expect.eql(o4, 1) and
      expect.eql(o5, 1) and
      expect.eql(o6, 0) and
      expect.eql(o7, 0)
  }
}
