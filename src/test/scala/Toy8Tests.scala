import cats.Show

object Toy8Tests extends weaver.FunSuite {

  given Show[Toy8.Control.ControlLines[Int]] = Show.show { o =>
    s"""(
      |ClockHalt=${o.ClockHalt},
      |AluAdd=${o.AluAdd},
      |AluXor=${o.AluXor},
      |AluAnd=${o.AluAnd},
      |RMuxAlu=${o.RMuxAlu},
      |RMuxMemory=${o.RMuxMemory},
      |RMuxIR=${o.RMuxIR},
      |RWrite=${o.RWrite},
      |IRWrite=${o.IRWrite},
      |PCLoad=${o.PCLoad},
      |PCIncrement=${o.PCIncrement},
      |PCWrite=${o.PCWrite},
      |AddrMuxPC=${o.AddrMuxPC},
      |AddrMuxIR=${o.AddrMuxIR},
      |MemoryWrite=${o.MemoryWrite}
      |)""".stripMargin
  }

  test("Toy8 Control OpCode Two, Add") {
    import Notation.`3-Bit Words`.OpCode
    import Notation.`8-Bit Words`.Ints.*
    expect.same(
      Toy8.Control(fetch = 1, fetchWrite = 0, execute = 0, executeWrite = 0, OpCode.Two, Zero),
      Toy8.Control.ControlLines[Int](
        ClockHalt = 0,
        AluAdd = 0,
        AluXor = 0,
        AluAnd = 0,
        RMuxAlu = 0,
        RMuxMemory = 0,
        RMuxIR = 0,
        RWrite = 0,
        IRWrite = 0,
        PCLoad = 0,
        PCIncrement = 1,
        PCWrite = 0,
        AddrMuxPC = 1,
        AddrMuxIR = 0,
        MemoryWrite = 0
      )
    ) and expect.same(
      Toy8.Control(fetch = 1, fetchWrite = 1, execute = 0, executeWrite = 0, OpCode.Two, Zero),
      Toy8.Control.ControlLines[Int](
        ClockHalt = 0,
        AluAdd = 0,
        AluXor = 0,
        AluAnd = 0,
        RMuxAlu = 0,
        RMuxMemory = 0,
        RMuxIR = 0,
        RWrite = 0,
        IRWrite = 1,
        PCLoad = 0,
        PCIncrement = 1,
        PCWrite = 0,
        AddrMuxPC = 1,
        AddrMuxIR = 0,
        MemoryWrite = 0
      )
    ) and expect.same(
      Toy8.Control(fetch = 0, fetchWrite = 0, execute = 1, executeWrite = 0, OpCode.Two, Zero),
      Toy8.Control.ControlLines[Int](
        ClockHalt = 0,
        AluAdd = 1,
        AluXor = 0,
        AluAnd = 0,
        RMuxAlu = 1,
        RMuxMemory = 0,
        RMuxIR = 0,
        RWrite = 0,
        IRWrite = 0,
        PCLoad = 0,
        PCIncrement = 1,
        PCWrite = 0,
        AddrMuxPC = 0,
        AddrMuxIR = 1,
        MemoryWrite = 0
      )
    ) and expect.same(
      Toy8.Control(fetch = 0, fetchWrite = 0, execute = 1, executeWrite = 1, OpCode.Two, Zero),
      Toy8.Control.ControlLines[Int](
        ClockHalt = 0,
        AluAdd = 1,
        AluXor = 0,
        AluAnd = 0,
        RMuxAlu = 1,
        RMuxMemory = 0,
        RMuxIR = 0,
        RWrite = 1,
        IRWrite = 0,
        PCLoad = 0,
        PCIncrement = 1,
        PCWrite = 1,
        AddrMuxPC = 0,
        AddrMuxIR = 1,
        MemoryWrite = 0
      )
    )
  }

  test("Toy8 Control OpCode Eight, BranchIfZero") {
    import Notation.`3-Bit Words`.OpCode
    import Notation.`8-Bit Words`.Ints.*
    expect.same(
      Toy8.Control(fetch = 1, fetchWrite = 0, execute = 0, executeWrite = 0, OpCode.Eight, Zero),
      Toy8.Control.ControlLines[Int](
        ClockHalt = 0,
        AluAdd = 0,
        AluXor = 0,
        AluAnd = 0,
        RMuxAlu = 0,
        RMuxMemory = 0,
        RMuxIR = 0,
        RWrite = 0,
        IRWrite = 0,
        PCLoad = 0,
        PCIncrement = 1,
        PCWrite = 0,
        AddrMuxPC = 1,
        AddrMuxIR = 0,
        MemoryWrite = 0
      )
    ) and expect.same(
      Toy8.Control(fetch = 1, fetchWrite = 1, execute = 0, executeWrite = 0, OpCode.Eight, Zero),
      Toy8.Control.ControlLines[Int](
        ClockHalt = 0,
        AluAdd = 0,
        AluXor = 0,
        AluAnd = 0,
        RMuxAlu = 0,
        RMuxMemory = 0,
        RMuxIR = 0,
        RWrite = 0,
        IRWrite = 1,
        PCLoad = 0,
        PCIncrement = 1,
        PCWrite = 0,
        AddrMuxPC = 1,
        AddrMuxIR = 0,
        MemoryWrite = 0
      )
    ) and expect.same(
      Toy8.Control(fetch = 0, fetchWrite = 0, execute = 1, executeWrite = 0, OpCode.Eight, Zero),
      Toy8.Control.ControlLines[Int](
        ClockHalt = 0,
        AluAdd = 0,
        AluXor = 0,
        AluAnd = 0,
        RMuxAlu = 0,
        RMuxMemory = 0,
        RMuxIR = 0,
        RWrite = 0,
        IRWrite = 0,
        PCLoad = 1,
        PCIncrement = 0,
        PCWrite = 0,
        AddrMuxPC = 0,
        AddrMuxIR = 0,
        MemoryWrite = 0
      )
    ) and expect.same(
      Toy8.Control(fetch = 0, fetchWrite = 0, execute = 1, executeWrite = 1, OpCode.Eight, Zero),
      Toy8.Control.ControlLines[Int](
        ClockHalt = 0,
        AluAdd = 0,
        AluXor = 0,
        AluAnd = 0,
        RMuxAlu = 0,
        RMuxMemory = 0,
        RMuxIR = 0,
        RWrite = 0,
        IRWrite = 0,
        PCLoad = 1,
        PCIncrement = 0,
        PCWrite = 1,
        AddrMuxPC = 0,
        AddrMuxIR = 0,
        MemoryWrite = 0
      )
    )
  }

  test("Toy8 Control OpCode Eight, Halt") {
    import Notation.`3-Bit Words`.OpCode
    import Notation.`8-Bit Words`.Ints.*
    expect.same(
      Toy8.Control(fetch = 1, fetchWrite = 0, execute = 0, executeWrite = 0, OpCode.One, Zero),
      Toy8.Control.ControlLines[Int](
        ClockHalt = 0,
        AluAdd = 0,
        AluXor = 0,
        AluAnd = 0,
        RMuxAlu = 0,
        RMuxMemory = 0,
        RMuxIR = 0,
        RWrite = 0,
        IRWrite = 0,
        PCLoad = 0,
        PCIncrement = 1,
        PCWrite = 0,
        AddrMuxPC = 1,
        AddrMuxIR = 0,
        MemoryWrite = 0
      )
    ) and expect.same(
      Toy8.Control(fetch = 1, fetchWrite = 1, execute = 0, executeWrite = 0, OpCode.One, Zero),
      Toy8.Control.ControlLines[Int](
        ClockHalt = 0,
        AluAdd = 0,
        AluXor = 0,
        AluAnd = 0,
        RMuxAlu = 0,
        RMuxMemory = 0,
        RMuxIR = 0,
        RWrite = 0,
        IRWrite = 1,
        PCLoad = 0,
        PCIncrement = 1,
        PCWrite = 0,
        AddrMuxPC = 1,
        AddrMuxIR = 0,
        MemoryWrite = 0
      )
    ) and expect.same(
      Toy8.Control(fetch = 0, fetchWrite = 0, execute = 1, executeWrite = 0, OpCode.One, Zero),
      Toy8.Control.ControlLines[Int](
        ClockHalt = 0,
        AluAdd = 0,
        AluXor = 0,
        AluAnd = 0,
        RMuxAlu = 0,
        RMuxMemory = 0,
        RMuxIR = 0,
        RWrite = 0,
        IRWrite = 0,
        PCLoad = 0,
        PCIncrement = 1,
        PCWrite = 0,
        AddrMuxPC = 0,
        AddrMuxIR = 0,
        MemoryWrite = 0
      )
    ) and expect.same(
      Toy8.Control(fetch = 0, fetchWrite = 0, execute = 1, executeWrite = 1, OpCode.One, Zero),
      Toy8.Control.ControlLines[Int](
        ClockHalt = 1,
        AluAdd = 0,
        AluXor = 0,
        AluAnd = 0,
        RMuxAlu = 0,
        RMuxMemory = 0,
        RMuxIR = 0,
        RWrite = 0,
        IRWrite = 0,
        PCLoad = 0,
        PCIncrement = 1,
        PCWrite = 1,
        AddrMuxPC = 0,
        AddrMuxIR = 0,
        MemoryWrite = 0
      )
    )
  }
}
