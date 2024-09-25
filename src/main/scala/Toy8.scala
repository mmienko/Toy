import BooleanFunctions.*
import Circuits.*
import Gates.*
import Modules.Clock.*
import Notation.*
import Signal.*
import Switch.*

/**
 * The first fundamental abstraction over physical power in a wire
 */
enum Signal {
  case On, Off
}

/**
 * The second fundamental abstraction over a physical switch, be a relay or transistor
 */
object Switch {
  def `input/off`[S: Notation](input: S, switchControl: S): S =
    ((input.toSignal, switchControl.toSignal) match
      case (Off, Off) => Off
      case (Off, On)  => Off
      case (On, Off)  => On
      case (On, On)   => Off
    ).as[S]

  // Input is always powered on
  def `on/off`[S: Notation](switchControl: S): S =
    `input/off`(input = On.as[S], switchControl)

}

/**
 * The logical abstraction to implement boolean algebra
 *
 */
object Gates {
  def Not[S: Notation](signal: S): S = `on/off`(signal)

  def Nor[S: Notation](x: S, y: S): S =
    `input/off`(
      input = `on/off`(x),
      switchControl = y
    )

  def Nor[S: Notation](x: S, y: S, xs: S*): S =
    xs.foldLeft(Nor(x, y))(`input/off`)

  def Or[S: Notation](x: S, y: S): S =
    Not(Nor(x, y))

  def Or[S: Notation](x: S, y: S, xs: S*): S =
    Not(Nor(x, y, xs*))

  def And[S: Notation](x: S, y: S): S =
    `input/off`(x, y.not)
//    Nor(Not(x), Not(y))

  def And[S: Notation](x: S, y: S, xs: S*): S =
    Nor(Not(x), Not(y), xs.map(Not[S].apply)*)

  def Xor[S: Notation](x: S, y: S): S =
    Or(
      And(x.not, y),
      And(x, y.not)
    )
}

/**
 * An extension over boolean algebra
 */
object BooleanFunctions {
  def Maj[S: Notation](x: S, y: S, z: S): S =
    Or(
      And(x.not, y, z),
      And(x, y.not, z),
      And(x, y, z.not),
      And(x, y, z)
    )

  def Odd[S: Notation](x: S, y: S, z: S): S =
    Or(
      And(x.not, y.not, z),
      And(x.not, y, z.not),
      And(x, y.not, z.not),
      And(x, y, z)
    )
}

/**
 * The next abstraction over groups of gates and other circuits. This covers combinational and sequential circuits.
 * The latter contains loops, which is used for memory.
 *
 * “A circuit is a gate or a network of circuits that are connected with wires, some of which are identified as inputs or outputs.”
 *
 * Excerpt From
 * Computer Science: An Interdisciplinary Approach
 * Robert Sedgewick
 */
object Circuits {

  object Adder {

    type Overflow[S] = S

    def withOverflow[S: Notation](
        x: `8-Bit Word`[S],
        y: `8-Bit Word`[S]
    ): (`8-Bit Word`[S], Overflow[S]) = {
      // initial carry is always 0, i.e. Off
      val `0` = summon[Notation[S]].off
      val c7  = Maj(x._8, y._8, `0`)
      val z8  = Odd(x._8, y._8, `0`)

      val c6 = Maj(x._7, y._7, c7)
      val z7 = Odd(x._7, y._7, c7)

      val c5 = Maj(x._6, y._6, c6)
      val z6 = Odd(x._6, y._6, c6)

      val c4 = Maj(x._5, y._5, c5)
      val z5 = Odd(x._5, y._5, c5)

      val c3 = Maj(x._4, y._4, c4)
      val z4 = Odd(x._4, y._4, c4)

      val c2 = Maj(x._3, y._3, c3)
      val z3 = Odd(x._3, y._3, c3)

      val c1 = Maj(x._2, y._2, c2)
      val z2 = Odd(x._2, y._2, c2)

      val c0 = Maj(x._1, y._1, c1)
      val z1 = Odd(x._1, y._1, c1)

      ((z1, z2, z3, z4, z5, z6, z7, z8), c0)
    }

    // ignores overflow
    def apply[S: Notation](x: `8-Bit Word`[S], y: `8-Bit Word`[S]): `8-Bit Word`[S] =
      withOverflow(x, y)._1
  }

  /**
   * A logical 2^n^-1 switch, i.e. it selects which of the n input lines to send to the output line via the control
   * lines.
   * `s` stands for "select", the control lines to select which input goes to output
   */
  object Mux {
    def `3-bit`[S: Notation](s: `3-Bit Word`[S])(input: `8-Bit Word`[S]): S =
      Or(
        And(And(s._1.not, s._2.not, s._3.not), input._1),
        And(And(s._1.not, s._2.not, s._3), input._2),
        And(And(s._1.not, s._2, s._3.not), input._3),
        And(And(s._1.not, s._2, s._3), input._4),
        And(And(s._1, s._2.not, s._3.not), input._5),
        And(And(s._1, s._2.not, s._3), input._6),
        And(And(s._1, s._2, s._3.not), input._7),
        And(And(s._1, s._2, s._3), input._8)
      )

    // Only one of the lines is always on
    def `3-way selection`[S: Notation](s: `3-Bit Word`[S])(input: `3-Bit Word`[S]): S =
      Or(
        And(And(s._1, s._2.not, s._3.not), input._1),
        And(And(s._1.not, s._2, s._3.not), input._2),
        And(And(s._1.not, s._2.not, s._3), input._3)
      )

    // Only one of the lines is always on
    def `2-way selection`[S: Notation](s: `2-Bit Word`[S])(input: `2-Bit Word`[S]): S =
      Or(
        And(And(s._1, s._2.not), input._1),
        And(And(s._1.not, s._2), input._2)
      )
  }

  /**
   * Circuit with n input wires and 2^n^ output wires.
   */
  object Decoder {
    def `3-bit`[S: Notation](s: `3-Bit Word`[S]): `8-Bit Word`[S] =
      (
        And(s._1.not, s._2.not, s._3.not),
        And(s._1.not, s._2.not, s._3),
        And(s._1.not, s._2, s._3.not),
        And(s._1.not, s._2, s._3),
        And(s._1, s._2.not, s._3.not),
        And(s._1, s._2.not, s._3),
        And(s._1, s._2, s._3.not),
        And(s._1, s._2, s._3)
      )
  }

  /**
   * A logical 1-2^n^ switch
   */
  object Demux {
    def `4-bit`[S: Notation](s: `4-Bit Word`[S])(v: S): `16-Bit Word`[S] =
      (
        And(And(s._1.not, s._2.not, s._3.not, s._4.not), v),
        And(And(s._1.not, s._2.not, s._3.not, s._4), v),
        And(And(s._1.not, s._2.not, s._3, s._4.not), v),
        And(And(s._1.not, s._2.not, s._3, s._4), v),
        And(And(s._1.not, s._2, s._3.not, s._4.not), v),
        And(And(s._1.not, s._2, s._3.not, s._4), v),
        And(And(s._1.not, s._2, s._3, s._4.not), v),
        And(And(s._1.not, s._2, s._3, s._4), v),
        And(And(s._1, s._2.not, s._3.not, s._4.not), v),
        And(And(s._1, s._2.not, s._3.not, s._4), v),
        And(And(s._1, s._2.not, s._3, s._4.not), v),
        And(And(s._1, s._2.not, s._3, s._4), v),
        And(And(s._1, s._2, s._3.not, s._4.not), v),
        And(And(s._1, s._2, s._3.not, s._4), v),
        And(And(s._1, s._2, s._3, s._4.not), v),
        And(And(s._1, s._2, s._3, s._4), v)
      )

    def `3-bit`[S: Notation](s: `3-Bit Word`[S])(v: S): `8-Bit Word`[S] =
      (
        And(And(s._1.not, s._2.not, s._3.not), v),
        And(And(s._1.not, s._2.not, s._3), v),
        And(And(s._1.not, s._2, s._3.not), v),
        And(And(s._1.not, s._2, s._3), v),
        And(And(s._1, s._2.not, s._3.not), v),
        And(And(s._1, s._2.not, s._3), v),
        And(And(s._1, s._2, s._3.not), v),
        And(And(s._1, s._2, s._3), v)
      )
  }

  // For each bit, choose which one using a 2-way selection mux
  def `4-bit 2-way BUS Mux`[S: Notation](
      s1: S,
      s2: S,
      bus1: `4-Bit Word`[S],
      bus2: `4-Bit Word`[S]
  ): `4-Bit Word`[S] =
    (
      Mux.`2-way selection`(s1, s2)(bus1._1, bus2._1),
      Mux.`2-way selection`(s1, s2)(bus1._2, bus2._2),
      Mux.`2-way selection`(s1, s2)(bus1._3, bus2._3),
      Mux.`2-way selection`(s1, s2)(bus1._4, bus2._4)
    )

  def `8-bit 3-way BUS Mux`[S: Notation](
      s1: S,
      s2: S,
      s3: S,
      bus1: `8-Bit Word`[S],
      bus2: `8-Bit Word`[S],
      bus3: `8-Bit Word`[S]
  ): `8-Bit Word`[S] =
    (
      Mux.`3-way selection`(s1, s2, s3)(bus1._1, bus2._1, bus3._1),
      Mux.`3-way selection`(s1, s2, s3)(bus1._2, bus2._2, bus3._2),
      Mux.`3-way selection`(s1, s2, s3)(bus1._3, bus2._3, bus3._3),
      Mux.`3-way selection`(s1, s2, s3)(bus1._4, bus2._4, bus3._4),
      Mux.`3-way selection`(s1, s2, s3)(bus1._5, bus2._5, bus3._5),
      Mux.`3-way selection`(s1, s2, s3)(bus1._6, bus2._6, bus3._6),
      Mux.`3-way selection`(s1, s2, s3)(bus1._7, bus2._7, bus3._7),
      Mux.`3-way selection`(s1, s2, s3)(bus1._8, bus2._8, bus3._8)
    )

  /**
   * A compress Decoder and Demux circuit, less switches than 2 separate circuits.
   *
   * @return (Decoder output, Demux output)
   */
  def `4-bit Decoder/Demux`[S: Notation](
      s: `4-Bit Word`[S]
  )(input: S): (`16-Bit Word`[S], `16-Bit Word`[S]) = {
    val decoderOutput = (
      And(s._1.not, s._2.not, s._3.not, s._4.not),
      And(s._1.not, s._2.not, s._3.not, s._4),
      And(s._1.not, s._2.not, s._3, s._4.not),
      And(s._1.not, s._2.not, s._3, s._4),
      And(s._1.not, s._2, s._3.not, s._4.not),
      And(s._1.not, s._2, s._3.not, s._4),
      And(s._1.not, s._2, s._3, s._4.not),
      And(s._1.not, s._2, s._3, s._4),
      And(s._1, s._2.not, s._3.not, s._4.not),
      And(s._1, s._2.not, s._3.not, s._4),
      And(s._1, s._2.not, s._3, s._4.not),
      And(s._1, s._2.not, s._3, s._4),
      And(s._1, s._2, s._3.not, s._4.not),
      And(s._1, s._2, s._3.not, s._4),
      And(s._1, s._2, s._3, s._4.not),
      And(s._1, s._2, s._3, s._4)
    )

    (
      decoderOutput,
      (
        And(decoderOutput._1, input),
        And(decoderOutput._2, input),
        And(decoderOutput._3, input),
        And(decoderOutput._4, input),
        And(decoderOutput._5, input),
        And(decoderOutput._6, input),
        And(decoderOutput._7, input),
        And(decoderOutput._8, input),
        And(decoderOutput._9, input),
        And(decoderOutput._10, input),
        And(decoderOutput._11, input),
        And(decoderOutput._12, input),
        And(decoderOutput._13, input),
        And(decoderOutput._14, input),
        And(decoderOutput._15, input),
        And(decoderOutput._16, input)
      )
    )
  }

  object Incrementer {
    def `4-bit`[S](x: `4-Bit Word`[S])(using sig: Notation[S]): `4-Bit Word`[S] = {
      val one = sig.on
      val c4  = And(x._4, one)
      val c3  = And(c4, x._3)
      val c2  = And(c3, x._2)
      (Xor(x._1, c2), Xor(x._2, c3), Xor(x._3, c4), Xor(x._4, one))
    }
  }

  /**
   * A simple circuit with stable Feedback useful for holding 1-bit state. It's initial output is [[Off]].
   */
  class FlipFlop[S](using Sig: Notation[S]) {
    private var resetOut     = Sig.on
    private var resetCoupled = Sig.off // Switch.`input/off`(resetOut, On)
    private var setOut       = Sig.on  // Switch.`on/off`(Off)
    private var setCoupled   = Sig.on  // Switch.`input/off`(resetCoupled, setOut)

    def apply(reset: S, set: S): S = {
      resetOut = Switch.`on/off`(reset)
      cycleSignalThroughCircuit()
      setOut = Switch.`on/off`(set)
      cycleSignalThroughCircuit()

      resetCoupled
    }

    private def cycleSignalThroughCircuit(): Unit = {
      setCoupled = Switch.`input/off`(setOut, resetCoupled)
      resetCoupled = Switch.`input/off`(resetOut, setCoupled)
    }

    def out: S = resetCoupled
  }

  /**
   * An elementary circuit for registers to hold and write single bits. It has the properties:
   *  - S if 1 if and only if the write control line is 1 and the input value is 1
   *  - R is 1 if and only if the write control line is 1 and the input value is 0
   */
  class RegisterBit[S: Notation] {
    private val flipFlop = FlipFlop[S]

    def apply(input: S, write: S): S =
      flipFlop(
        reset = And(write, input.not),
        set = And(write, input)
      )

    def out: S = flipFlop.out
  }

}

object Modules {
  def ALU[S: Notation](
      x: `8-Bit Word`[S],
      y: `8-Bit Word`[S],
      selectAdd: S,
      selectXor: S,
      selectAnd: S
  ): `8-Bit Word`[S] = {
    val added = Adder(x, y)
    val mux   = Mux.`3-way selection`(selectAdd, selectXor, selectAnd)
    (
      mux(added._1, Xor(x._1, y._1), And(x._1, y._1)),
      mux(added._2, Xor(x._2, y._2), And(x._2, y._2)),
      mux(added._3, Xor(x._3, y._3), And(x._3, y._3)),
      mux(added._4, Xor(x._4, y._4), And(x._4, y._4)),
      mux(added._5, Xor(x._5, y._5), And(x._5, y._5)),
      mux(added._6, Xor(x._6, y._6), And(x._6, y._6)),
      mux(added._7, Xor(x._7, y._7), And(x._7, y._7)),
      mux(added._8, Xor(x._8, y._8), And(x._8, y._8))
    )
  }

  object Register {
    import Circuits.RegisterBit as RB
    class `4-bit`[S: Notation] {
      private val registers = (RB[S](), RB[S](), RB[S](), RB[S]())

      def apply(write: S, word: `4-Bit Word`[S]): `4-Bit Word`[S] =
        (
          registers._1(word._1, write),
          registers._2(word._2, write),
          registers._3(word._3, write),
          registers._4(word._4, write)
        )
    }

    class `8-bit`[S: Notation] {
      private val registers =
        (RB[S](), RB[S](), RB[S](), RB[S](), RB[S](), RB[S](), RB[S](), RB[S]())

      def apply(write: S, input: `8-Bit Word`[S]): `8-Bit Word`[S] =
        (
          registers._1(input._1, write),
          registers._2(input._2, write),
          registers._3(input._3, write),
          registers._4(input._4, write),
          registers._5(input._5, write),
          registers._6(input._6, write),
          registers._7(input._7, write),
          registers._8(input._8, write)
        )
    }

  }

  /**
   * A circuit that takes pulses from a physical source and converts them to periodic instructions that control
   * a computer. Pulses, a.k.a ticks, need to be sufficiently long to activate any switch and the time between ticks
   * need is longer than the longest chain of switches within the circuit.
   */
  class Clock[S: Notation] {

    import Clock.*

    private val register = Circuits.RegisterBit[S]()
    private var execute  = register.out
    private var fetch    = execute.not

    def apply(write: S): TickTock[S] = {
      val tickStart = (fetch, And(fetch, write), execute, And(execute, write))
      execute = register(input = fetch, write)
      fetch = execute.not
      val tickEnd = (fetch, Notation[S].off, execute, Notation[S].off)
      (tickStart, tickEnd)
    }
  }

  object Clock {
    type Fetch[S]        = S
    type FetchWrite[S]   = S
    type Execute[S]      = S
    type ExecuteWrite[S] = S
    type Tick[S]         = (Fetch[S], FetchWrite[S], Execute[S], ExecuteWrite[S])
    type TickTock[S]     = (Tick[S], Tick[S])
  }

  object ProgramCounter {
    class `4-bit`[S](using Sig: Notation[S]) {
      private val register = Register.`4-bit`[S]()
      private var incrementerOutput = incrementByOne(
        register(Sig.off, (Sig.off, Sig.off, Sig.off, Sig.off))
      )

      def apply(input: `4-Bit Word`[S], increment: S, load: S, write: S): `4-Bit Word`[S] = {
        val pc = register(
          write,
          word = Circuits.`4-bit 2-way BUS Mux`(increment, load, incrementerOutput, input)
        )
        incrementerOutput = incrementByOne(pc)
        pc
      }

      private def incrementByOne = Circuits.Incrementer.`4-bit`[S]

    }
  }

  object Memory {
    private class MemoryBit[S: Notation] {

      private val registerBit = RegisterBit[S]()

      def apply(input: S, write: S, read: S): S =
        And(registerBit(input, write), read)
    }

    private type MemBit[S] = MemoryBit[S]
    private type `8-bit Memory Word`[S] =
      (MemBit[S], MemBit[S], MemBit[S], MemBit[S], MemBit[S], MemBit[S], MemBit[S], MemBit[S])

    private def MB[S: Notation]() = MemoryBit()

    class `16-word 8-bit`[S: Notation] {
      private val registers = (
        (MB(), MB(), MB(), MB(), MB(), MB(), MB(), MB()), // 1
        (MB(), MB(), MB(), MB(), MB(), MB(), MB(), MB()), // 2
        (MB(), MB(), MB(), MB(), MB(), MB(), MB(), MB()), // 3
        (MB(), MB(), MB(), MB(), MB(), MB(), MB(), MB()), // 4
        (MB(), MB(), MB(), MB(), MB(), MB(), MB(), MB()), // 5
        (MB(), MB(), MB(), MB(), MB(), MB(), MB(), MB()), // 6
        (MB(), MB(), MB(), MB(), MB(), MB(), MB(), MB()), // 7
        (MB(), MB(), MB(), MB(), MB(), MB(), MB(), MB()), // 8
        (MB(), MB(), MB(), MB(), MB(), MB(), MB(), MB()), // 9
        (MB(), MB(), MB(), MB(), MB(), MB(), MB(), MB()), // 10
        (MB(), MB(), MB(), MB(), MB(), MB(), MB(), MB()), // 11
        (MB(), MB(), MB(), MB(), MB(), MB(), MB(), MB()), // 12
        (MB(), MB(), MB(), MB(), MB(), MB(), MB(), MB()), // 13
        (MB(), MB(), MB(), MB(), MB(), MB(), MB(), MB()), // 14
        (MB(), MB(), MB(), MB(), MB(), MB(), MB(), MB()), // 15
        (MB(), MB(), MB(), MB(), MB(), MB(), MB(), MB())  // 16
      )

      def apply(
          input: `8-Bit Word`[S],
          address: `4-Bit Word`[S],
          write: S
      ): `8-Bit Word`[S] = {
        val (readSignals, writeSignals) =
          Circuits.`4-bit Decoder/Demux`(address._1, address._2, address._3, address._4)(write)
        val updateCol = updateColumn(writeSignals, readSignals)(_, _)

        (
          updateCol(input._1, _._1),
          updateCol(input._2, _._2),
          updateCol(input._3, _._3),
          updateCol(input._4, _._4),
          updateCol(input._5, _._5),
          updateCol(input._6, _._6),
          updateCol(input._7, _._7),
          updateCol(input._8, _._8)
        )
      }

      private def updateColumn(writeSignals: `16-Bit Word`[S], readSignals: `16-Bit Word`[S])(
          input: S,
          selectMemoryBitFromWord: `8-bit Memory Word`[S] => MemoryBit[S]
      ): S =
        Or(
          selectMemoryBitFromWord(registers._1)(input, writeSignals._1, readSignals._1),
          selectMemoryBitFromWord(registers._2)(input, writeSignals._2, readSignals._2),
          selectMemoryBitFromWord(registers._3)(input, writeSignals._3, readSignals._3),
          selectMemoryBitFromWord(registers._4)(input, writeSignals._4, readSignals._4),
          selectMemoryBitFromWord(registers._5)(input, writeSignals._5, readSignals._5),
          selectMemoryBitFromWord(registers._6)(input, writeSignals._6, readSignals._6),
          selectMemoryBitFromWord(registers._7)(input, writeSignals._7, readSignals._7),
          selectMemoryBitFromWord(registers._8)(input, writeSignals._8, readSignals._8),
          selectMemoryBitFromWord(registers._9)(input, writeSignals._9, readSignals._9),
          selectMemoryBitFromWord(registers._10)(input, writeSignals._10, readSignals._10),
          selectMemoryBitFromWord(registers._11)(input, writeSignals._11, readSignals._11),
          selectMemoryBitFromWord(registers._12)(input, writeSignals._12, readSignals._12),
          selectMemoryBitFromWord(registers._13)(input, writeSignals._13, readSignals._13),
          selectMemoryBitFromWord(registers._14)(input, writeSignals._14, readSignals._14),
          selectMemoryBitFromWord(registers._15)(input, writeSignals._15, readSignals._15),
          selectMemoryBitFromWord(registers._16)(input, writeSignals._16, readSignals._16)
        )
    }
  }
}

/**
 * An instruction:
 * [_,_,_]0[_,_,_,_]
 * opcode,0,addr
 *
 * opcode hex  description     pseudo-code
 * 0000   0    halt
 * 0010   2    add              R = R + M[addr]
 * 0100   4    bitwise and      R = R & M[addr]
 * 0110   6    bitwise xor      R = R ^^ M[addr]
 * 1000   8    load address     R = addr
 * 1010   A    load             R = M[addr]
 * 1100   C    store            M[addr] = R
 * 1110   E    branch if zero   if (R == 0) PC = addr
 *
 * Memory location F is input/output
 * Memory location 0 is always 0
 */
class Toy8[S: Notation](trace: Boolean = false) {
  import Circuits.{Incrementer, `4-bit 2-way BUS Mux`, `8-bit 3-way BUS Mux`}
  import Modules.*
  import Toy8.Control

  private type Halt = S

  private val OffSignal = Notation[S].off
  private val OnSignal  = Notation[S].on
  private val InitialBusValues =
    (OffSignal, OffSignal, OffSignal, OffSignal, OffSignal, OffSignal, OffSignal, OffSignal)
  private val AddressZero = (OffSignal, OffSignal, OffSignal, OffSignal)

  private val memory              = Memory.`16-word 8-bit`[S]
  private val programCounter      = ProgramCounter.`4-bit`[S]
  private val clock               = Clock[S]
  private val register            = Register.`8-bit`[S]
  private val instructionRegister = Register.`8-bit`[S]
  private var IROut               = InitialBusValues
  private var RegisterOut         = InitialBusValues
  private var MemOut              = InitialBusValues

  /**
   * A specialized circuit that stores 8-bit words into memory
   * @param program a sequence of b-bit words representing the program
   */
  def loadProgram(program: `8-Bit Word`[S]*): Unit = {
    program.foldLeft(AddressZero) { case (addr, instruction) =>
      val nextAddr = Incrementer.`4-bit`(addr)
      memory(
        input = instruction,
        address = nextAddr,
        write = OnSignal
      )
      nextAddr
    }
    programCounter(input = AddressZero, increment = OnSignal, load = OffSignal, write = OnSignal)
  }

  def read(address: `4-Bit Word`[S]): `8-Bit Word`[S] =
    memory(input = InitialBusValues, address, write = OffSignal)

  def dump(): Unit = {
    val `0` = OffSignal
    val `1` = OnSignal
    println(read(address = (`0`, `0`, `0`, `0`)))
    println(read(address = (`0`, `0`, `0`, `1`)))
    println(read(address = (`0`, `0`, `1`, `0`)))
    println(read(address = (`0`, `0`, `1`, `1`)))
    println(read(address = (`0`, `1`, `0`, `0`)))
    println(read(address = (`0`, `1`, `0`, `1`)))
    println(read(address = (`0`, `1`, `1`, `0`)))
    println(read(address = (`0`, `1`, `1`, `1`)))
  }

  /**
   * Represents a physical clock spinning around, briefly setting the "write" signal on
   * the logical clock, [[Modules.Clock]], to [[Signal.On]]. For the remainder of the time,
   * the logical clock sees a "write" signal of [[Signal.Off]]. There is enough time
   * for the write signal to propagate through the rest of the circuit.
   *
   * A computer basically works in cycles of fetch an instruction from memory into IR, then execute the instruction.
   * The next instruction is an incremented address or jump address stored in PC (from previous execute). Fetch write
   * does the storing of instruction into IR. An instruction could point to another place in memory. The execute
   * phase reads from memory, powers the appropriate circuits, then does the write to registers, memory, and updates
   * to PC. Finally, there could be a halt issued which stops the clock and machine.
   *
   * Higher layers, i.e. machine code, just care that a sequence of instructions, over some data, is executed.
   */
  def run(): Unit = {
    import scala.util.control.Breaks.*
    breakable(while (true) {
      // The api could be cleaner, don't need to propagate twice during Off signal.
      val (power1, _) = clock(write = OffSignal)
      propagate(power1)

      val (tick, tock) = clock(write = OnSignal)
      if (propagate(tick) == OnSignal)
        break()
      propagate(tock)
    })
  }

  private def propagate(tick: Tick[S]): Halt = {
    val opcode = (IROut._1, IROut._2, IROut._3)

    val controlLines = Control[S](
      fetch = tick._1,
      fetchWrite = tick._2,
      execute = tick._3,
      executeWrite = tick._4,
      opcode = opcode,
      input = RegisterOut
    )
    val addr = (IROut._5, IROut._6, IROut._7, IROut._8)

    val pc = programCounter(
      input = addr,
      increment = controlLines.PCIncrement,
      load = controlLines.PCLoad,
      write = controlLines.PCWrite
    )
    MemOut = memory(
      input = RegisterOut,
      address = `4-bit 2-way BUS Mux`(
        controlLines.AddrMuxPC,
        controlLines.AddrMuxIR,
        pc,
        addr
      ),
      write = controlLines.MemoryWrite
    )

    IROut = instructionRegister(write = controlLines.IRWrite, MemOut)

    RegisterOut = register(
      write = controlLines.RWrite,
      input = `8-bit 3-way BUS Mux`(
        controlLines.RMuxIR,
        controlLines.RMuxMemory,
        controlLines.RMuxAlu,
        bus1 = (OffSignal, OffSignal, OffSignal, OffSignal, IROut._5, IROut._6, IROut._7, IROut._8),
        bus2 = MemOut,
        bus3 = ALU(
          RegisterOut,
          MemOut,
          selectAdd = controlLines.AluAdd,
          selectXor = controlLines.AluXor,
          selectAnd = controlLines.AluAnd
        )
      )
    )

    if (trace) {
      println()
      println(s"TICK $tick")
      println(s"OPCODE $opcode")
      println(s"CONTROL $controlLines")
      println(s"PC $pc")
      println(s"M $MemOut")
      println(s"IR $IROut")
      println(s"R $RegisterOut")
    }

    controlLines.ClockHalt
  }
}

object Toy8 {
  object Control {
    final case class ControlLines[S: Notation](
        ClockHalt: S,
        AluAdd: S,
        AluXor: S,
        AluAnd: S,
        RMuxAlu: S,
        RMuxMemory: S,
        RMuxIR: S,
        RWrite: S,
        IRWrite: S,
        PCLoad: S,
        PCIncrement: S,
        PCWrite: S,
        AddrMuxPC: S,
        AddrMuxIR: S,
        MemoryWrite: S
    ) {
      override def toString: String = {
        val body = List(
          ("ClockHalt=", ClockHalt),
          ("AluAdd=", AluAdd),
          ("AluXor=", AluXor),
          ("AluAnd=", AluAnd),
          ("RMuxAlu=", RMuxAlu),
          ("RMuxMemory=", RMuxMemory),
          ("RMuxIR=", RMuxIR),
          ("RWrite=", RWrite),
          ("IRWrite=", IRWrite),
          ("PCLoad=", PCLoad),
          ("PCIncrement=", PCIncrement),
          ("PCWrite=", PCWrite),
          ("AddrMuxPC=", AddrMuxPC),
          ("AddrMuxIR=", AddrMuxIR),
          ("MemoryWrite=", MemoryWrite)
        ).filter(_._2.toSignal == Signal.On)
          .map((name, value) => name + value)
          .mkString(" ")
        s"($body)"
      }
    }

    def apply[S: Notation](
        fetch: Fetch[S],
        fetchWrite: FetchWrite[S],
        execute: Execute[S],
        executeWrite: ExecuteWrite[S],
        opcode: `3-Bit Word`[S],
        input: `8-Bit Word`[S]
    ): ControlLines[S] = {
      val op             = Circuits.Decoder.`3-bit`(opcode)
      val haltOp         = And(executeWrite, op._1)
      val addOp          = And(execute, op._2)
      val xorOp          = And(execute, op._3)
      val andOp          = And(execute, op._4)
      val loadAddressOp  = And(execute, op._5)
      val loadOp         = And(execute, op._6)
      val storeOp        = And(execute, op._7)
      val branchIfZeroOp = And(execute, op._8)
      val RMuxAlu        = Or(addOp, xorOp, andOp)
      val PCLoad = And(
        Nor(input._1, input._2, input._3, input._4, input._5, input._6, input._7, input._8),
        branchIfZeroOp
      )

      ControlLines(
        AddrMuxPC = fetch,
        IRWrite = fetchWrite,
        // execute
        AluAdd = addOp,
        AluXor = xorOp,
        AluAnd = andOp,
        RMuxAlu = RMuxAlu,
        RMuxIR = loadAddressOp,
        RMuxMemory = loadOp,
        PCLoad = PCLoad,
        PCIncrement = PCLoad.not,
        AddrMuxIR = Or(RMuxAlu, loadOp, storeOp),
        // ExecuteWrite
        PCWrite = executeWrite,
        ClockHalt = haltOp,
        RWrite = Or(
          And(executeWrite, op._2),
          And(executeWrite, op._3),
          And(executeWrite, op._4),
          And(executeWrite, op._5),
          And(executeWrite, op._6)
        ),
        MemoryWrite = And(executeWrite, op._7)
      )
    }
  }
}

//@main
def toy8Main(): Unit = {
  val toy = Toy8[Int](trace = false)
  toy.loadProgram(
    /* OPCODE    /**/   ADDRESS       # COMMAND  PSEUDOCODE */
    (1, 0, 1, 0, /**/ 0, 1, 0, 1), // 1   A5     R = M[5]
    (0, 0, 1, 0, /**/ 0, 1, 1, 0), // 2   26     R = R + M[6]
    (1, 1, 0, 0, /**/ 0, 1, 1, 1), // 3   C7     M[7] = R
    (0, 0, 0, 0, /**/ 0, 0, 0, 0), // 4   00     halt
    (0, 0, 0, 0, /**/ 1, 0, 0, 0), // 5   08
    (0, 0, 0, 0, /**/ 0, 1, 0, 1), // 6   05
    (0, 0, 0, 0, /**/ 0, 0, 0, 0)  // 7   00
  )
  println("Initial Memory")
  toy.dump()
  println("Run program")
  toy.run()
  println("Final Memory")
  toy.dump()
}
