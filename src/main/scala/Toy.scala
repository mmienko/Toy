import scala.annotation.switch
import scala.io.Source
import scala.util.control.Breaks.{break, breakable}

class Toy {
  private val R  = Array.fill(16)(0)
  private val M  = Array.fill(256)(0)
  private var PC = 0

  def run(): Unit = breakable {
    val stdInput = Source.stdin.getLines()
//    println("running")
    while (true) {
      val IR = M(PC) // Fetch
//      println(s"${PC.toHexString} : ${IR.toHexString}")
      PC = (PC + 1) & 0xff // Increment (0xFF chops off bits above short range, 255)
      // Execute
      val op   = (IR >> 12) & 0xf
      val d    = (IR >> 8) & 0xf
      val s    = (IR >> 4) & 0xf
      val t    = (IR >> 0) & 0xf
      val addr = (IR >> 0) & 0xff

      if (op == 0) break()

      stdin(addr, op, t, stdInput)

      (op: @switch) match
        case 1  => R(d) = R(s) + R(t)
        case 2  => R(d) = R(s) - R(t)
        case 3  => R(d) = R(s) & R(t)
        case 4  => R(d) = R(s) ^ R(t)
        case 5  => R(d) = R(s) << R(t)
        case 6  => R(d) = (R(s) >> R(t)).shortValue
        case 7  => R(d) = addr
        case 8  => R(d) = M(addr)
        case 9  => M(addr) = R(d)
        case 10 => R(d) = M(R(t) & 0xff)
        case 11 => M(R(t) & 0xff) = R(d)
        case 12 => if (R(d).shortValue == 0) PC = addr
        case 13 => if (R(d).shortValue > 0) PC = addr
        case 14 => PC = R(d) & 0xff
        case 15 => R(d) = PC; PC = addr

      stdout(addr, op, t)
      R(d) &= 0xffff
      R(0) = 0
    }
  }

  private def stdin(addr: Int, op: Int, t: Int, stdInput: Iterator[String]) = {
    if ((addr == 0xff && op == 8) || (R(t) == 0xff && op == 10)) {
      M(0xff) = Integer.parseInt(stdInput.next(), 16) & 0xffff
//      println(s"READ ${M(0xFF).toHexString}")
    }
  }

  private def stdout(addr: Int, op: Int, t: Int) =
    if ((addr == 0xff && op == 9) || (R(t) == 0xff && op == 11))
      System.out.printf("%04X\n", M(0xff))
}

object Toy {
  def load(fileName: String, programCounter: Int): Toy = {
    val toy = Toy()
    toy.PC = programCounter & 0xff

    val source = Source.fromFile(fileName)
    try
      source.getLines().foreach { line =>
        val parts = line.split(":\\s+")
//        println(s"${line} split to ${parts(0)}: ${parts(1)}")
        val addr  = Integer.parseInt(parts(0), 16) & 0xff
        val value = Integer.parseInt(parts(1), 16) & 0xffff
        toy.M(addr) = value
      }
    finally
      source.close()

    toy
  }

  def load(fileName: String): Toy = {
    val toy = Toy()
    toy.PC = 0x10

    val source = Source.fromFile(fileName)
    try
      var addr  = toy.PC
      val lines = source.getLines()
      while (addr < 0xff && lines.nonEmpty) {
        val instruction = Integer.parseInt(lines.next(), 16) & 0xffff
//        println(s"${addr.toHexString} : ${instruction.toHexString}")
        toy.M(addr) = instruction
        addr += 1
      }
    finally source.close()

    toy
  }
}

// scala src/main/scala/Toy.scala programs/add-stdin.toy < programs/add-stdin.txt
@main
def toyMain(fileName: String): Unit =
  Toy.load(fileName).run()

// Another way of running it
// Run below and give contents of R[2], register # 2. Uncomment print statements.
//scala src/main/scala/Toy.scala programs/other.toy 10
//@main
//def toyMain(fileName: String, programCounter: String): Unit = {
//  Toy.load(
//    fileName,
//    programCounter = Integer.parseInt(programCounter, 16)
//  ).run()
//}
