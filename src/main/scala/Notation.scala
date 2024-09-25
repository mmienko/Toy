import Gates.Not
import Signal.{Off, On}

trait Notation[A] {
  def toSignal(a: A): Signal

  def fromSignal(signal: Signal): A

  def off: A

  def on: A
}

object Notation {
  type `2-Bit Word`[S]  = (S, S)
  type `3-Bit Word`[S]  = (S, S, S)
  type `4-Bit Word`[S]  = (S, S, S, S)
  type `8-Bit Word`[S]  = (S, S, S, S, S, S, S, S)
  type `16-Bit Word`[S] = (S, S, S, S, S, S, S, S, S, S, S, S, S, S, S, S)

  def apply[S: Notation]: Notation[S] = summon[Notation[S]]

  def instance[A](func1: A => Signal, func2: Signal => A): Notation[A] = new Notation[A] {
    override def toSignal(a: A): Signal = func1(a)

    override def fromSignal(signal: Signal): A = func2(signal)

    override lazy val off: A = fromSignal(Off)

    override lazy val on: A = fromSignal(On)
  }

  extension [A: Notation](a: A) {
    def toSignal: Signal = Notation[A].toSignal(a)
    def not: A = toSignal.not.as[A]
  }

  extension (s: Signal) {
    def as[A: Notation]: A = Notation[A].fromSignal(s)
  }

  extension (s: Signal) {
    def not: Signal = Not(s)
  }

  given Notation[Signal] = instance(identity, identity)

  given Notation[Int] = instance(
    func1 = i => if (i > 0) On else Off,
    func2 = {
      case On => 1
      case Off => 0
    }
  )

  given Notation[Boolean] = instance(
    b => if (b) On else Off,
    {
      case On => true
      case Off => false
    }
  )

  given Notation[Char] = instance(
    c => if (c == 't') On else Off,
    {
      case On => 't'
      case Off => 'f'
    }
  )

  object `3-Bit Words` {
    val Zero: `3-Bit Word`[Signal] = (Off, Off, Off)

    object Ints {
      val Zero: `3-Bit Word`[Int]  = (0, 0, 0)
      val One: `3-Bit Word`[Int]   = (0, 0, 1)
      val Two: `3-Bit Word`[Int]   = (0, 1, 0)
      val Three: `3-Bit Word`[Int] = (0, 1, 1)
      val Four: `3-Bit Word`[Int]  = (1, 0, 0)
      val Five: `3-Bit Word`[Int]  = (1, 0, 1)
      val Six: `3-Bit Word`[Int]   = (1, 1, 0)
      val Seven: `3-Bit Word`[Int] = (1, 1, 1)
    }
    object OpCode {
      val One: `3-Bit Word`[Int]   = (0, 0, 0)
      val Two: `3-Bit Word`[Int]   = (0, 0, 1)
      val Three: `3-Bit Word`[Int] = (0, 1, 0)
      val Four: `3-Bit Word`[Int]  = (0, 1, 1)
      val Five: `3-Bit Word`[Int]  = (1, 0, 0)
      val Six: `3-Bit Word`[Int]   = (1, 0, 1)
      val Seven: `3-Bit Word`[Int] = (1, 1, 0)
      val Eight: `3-Bit Word`[Int] = (1, 1, 1)
    }

  }

  object `4-Bit Words` {
    val Zero: `4-Bit Word`[Signal] = (Off, Off, Off, Off)

    object Ints {
      val Zero: `4-Bit Word`[Int]  = (0, 0, 0, 0)
      val One: `4-Bit Word`[Int]   = (0, 0, 0, 1)
      val Two: `4-Bit Word`[Int]   = (0, 0, 1, 0)
      val Three: `4-Bit Word`[Int] = (0, 0, 1, 1)
    }
  }

  object `8-Bit Words` {
    object Ints {
      val Zero: `8-Bit Word`[Int]  = (0, 0, 0, 0, 0, 0, 0, 0)
      val One: `8-Bit Word`[Int]   = (0, 0, 0, 0, 0, 0, 0, 1)
      val Two: `8-Bit Word`[Int]   = (0, 0, 0, 0, 0, 0, 1, 0)
      val Three: `8-Bit Word`[Int] = (0, 0, 0, 0, 0, 0, 1, 1)
    }
  }

}
