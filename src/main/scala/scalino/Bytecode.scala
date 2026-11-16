package scalino

/** A program for the machine: C = i0, i1, …, ik−1. */
type Code = Vector[Instr]

/** The instructions of the stack machine. */
enum Instr:
  case Push(n: Value)
  case Load(x: String)
  case Store(x: String)

  /** ADD, SUB and MUL: the OP of rule M-Op. */
  case Op(op: ArithOp)
  case Div
  case Lt
  case Print
  case Jmp(target: Int)
  case Jmpz(target: Int)

  /** The textual form, as written in `.scalinobc` files. */
  def show: String = this match
    case Push(n)         => s"PUSH $n"
    case Load(x)         => s"LOAD $x"
    case Store(x)        => s"STORE $x"
    case Op(ArithOp.Add) => "ADD"
    case Op(ArithOp.Sub) => "SUB"
    case Op(ArithOp.Mul) => "MUL"
    case Div             => "DIV"
    case Lt              => "LT"
    case Print           => "PRINT"
    case Jmp(target)     => s"JMP $target"
    case Jmpz(target)    => s"JMPZ $target"

/** The `.scalinobc` format: plain text with one instruction per line, so line i + 1 holds C[i]. */
object Bytecode:
  import Instr.*

  def write(code: Code): String = code.map(instr => s"${instr.show}\n").mkString

  def read(text: String): Either[StaticError, Code] =
    val lines = text.linesIterator.toVector.zipWithIndex.map: (line, i) =>
      parseLine(line.trim).left.map(StaticError(Pos(i + 1, 1), _))
    lines.collectFirst { case Left(error) => error }.toLeft(lines.collect { case Right(instr) => instr })

  private val Integer = """-?\d+""".r

  private object Name:
    def unapply(s: String): Option[String] = Option.when(Lexer.isIdentifier(s))(s)

  private object Index:
    def unapply(s: String): Option[Int] = s.toIntOption.filter(_ >= 0)

  private def parseLine(line: String): Either[String, Instr] = line.split("\\s+").toList match
    case List("")                    => Left("empty line, expected an instruction")
    case List("PUSH", n @ Integer()) => Right(Push(BigInt(n)))
    case List("LOAD", Name(x))       => Right(Load(x))
    case List("STORE", Name(x))      => Right(Store(x))
    case List("ADD")                 => Right(Op(ArithOp.Add))
    case List("SUB")                 => Right(Op(ArithOp.Sub))
    case List("MUL")                 => Right(Op(ArithOp.Mul))
    case List("DIV")                 => Right(Div)
    case List("LT")                  => Right(Lt)
    case List("PRINT")               => Right(Print)
    case List("JMP", Index(l))       => Right(Jmp(l))
    case List("JMPZ", Index(l))      => Right(Jmpz(l))
    case _                           => Left(s"invalid instruction '$line'")
