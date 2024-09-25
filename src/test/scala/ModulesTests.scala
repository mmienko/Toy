object ModulesTests extends weaver.FunSuite {

  test("ALU") {
    import Notation.`8-Bit Words`.Ints.*
    expect.eql( // Test Add
      Modules.ALU(Zero, One, selectAdd = 1, selectXor = 0, selectAnd = 0),
      One
    ) and expect.eql(
      Modules.ALU(One, One, selectAdd = 1, selectXor = 0, selectAnd = 0),
      Two
    ) and expect.eql( // Test XOR
      Modules.ALU(One, One, selectAdd = 0, selectXor = 1, selectAnd = 0),
      Zero
    ) and expect.eql(
      Modules.ALU(Zero, One, selectAdd = 0, selectXor = 1, selectAnd = 0),
      One
    ) and expect.eql(
      Modules.ALU(Two, One, selectAdd = 0, selectXor = 1, selectAnd = 0),
      Three
    ) and expect.eql( // Test AND
      Modules.ALU(One, Zero, selectAdd = 0, selectXor = 0, selectAnd = 1),
      Zero
    ) and expect.eql(
      Modules.ALU(One, Two, selectAdd = 0, selectXor = 0, selectAnd = 1),
      Zero
    ) and expect.eql(
      Modules.ALU(One, One, selectAdd = 0, selectXor = 0, selectAnd = 1),
      One
    )
  }

  test("Register") {
    import Notation.`4-Bit Words`.Ints.*
    val r4 = Modules.Register.`4-bit`[Int]
    expect.eql(r4(write = 0, word = Zero), Zero) and
      expect.eql(r4(write = 0, word = One), Zero) and
      expect.eql(r4(write = 1, word = One), One) and
      expect.eql(r4(write = 1, word = Three), Three) and
      expect.eql(r4(write = 0, word = Zero), Three)
  }

  test("Clock") {
    val c = Modules.Clock[Int]
    def cycle =
      expect.eql(c(write = 0), ((1, 0, 0, 0), (1, 0, 0, 0))) and
        expect.eql(c(write = 1), ((1, 1, 0, 0), (0, 0, 1, 0))) and
        expect.eql(c(write = 0), ((0, 0, 1, 0), (0, 0, 1, 0))) and
        expect.eql(c(write = 0), ((0, 0, 1, 0), (0, 0, 1, 0))) and
        expect.eql(c(write = 1), ((0, 0, 1, 1), (1, 0, 0, 0))) and
        // second cycle
        expect.eql(c(write = 0), ((1, 0, 0, 0), (1, 0, 0, 0))) and
        expect.eql(c(write = 1), ((1, 1, 0, 0), (0, 0, 1, 0))) and
        expect.eql(c(write = 0), ((0, 0, 1, 0), (0, 0, 1, 0))) and
        expect.eql(c(write = 1), ((0, 0, 1, 1), (1, 0, 0, 0)))

    cycle and cycle and cycle
  }

  test("Program Counter") {
    val pc = Modules.ProgramCounter.`4-bit`[Int]
    import Notation.`4-Bit Words`.Ints.*
    // increment
    expect.eql(pc(input = Zero, increment = 1, load = 0, write = 0), Zero) and
      expect.eql(pc(input = Zero, increment = 1, load = 0, write = 1), One) and
      expect.eql(pc(input = Zero, increment = 1, load = 0, write = 0), One) and
      expect.eql(pc(input = Zero, increment = 1, load = 0, write = 1), Two) and
      expect.eql(pc(input = One, increment = 1, load = 0, write = 1), Three) and
      // load
      expect.eql(pc(input = One, increment = 0, load = 1, write = 1), One) and
      expect.eql(pc(input = One, increment = 1, load = 0, write = 1), Two)
  }

  test("Memory") {
    import Notation.`8-Bit Words`.{Ints => Words}
    import Notation.`4-Bit Words`.{Ints => Addr}
    val mem = Modules.Memory.`16-word 8-bit`[Int]()
    // read empty
    expect.eql(mem(input = Words.Zero, address = Addr.Zero, write = 0), Words.Zero) and
      expect.eql(mem(input = Words.Zero, address = Addr.Three, write = 0), Words.Zero) and
      // set
      expect.eql(mem(input = Words.Two, address = Addr.Three, write = 1), Words.Two) and
      expect.eql(mem(input = Words.One, address = Addr.Two, write = 1), Words.One) and
      // read
      expect.eql(mem(input = Words.Zero, address = Addr.Three, write = 0), Words.Two) and
      expect.eql(mem(input = Words.Zero, address = Addr.Two, write = 0), Words.One)
  }
}
