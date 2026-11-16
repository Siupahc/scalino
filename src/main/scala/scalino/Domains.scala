package scalino

// Semantic domains.

/** The values of Scalino: mathematical integers, so arithmetic never overflows. */
type Value = BigInt

/** σ ∈ Store, a finite partial map from variables to integers. */
type Store = Map[String, Value]

/** ω ∈ Out = Int*, the integers printed so far: ω · n is `out :+ n`. */
type Out = Vector[Value]

/** Why no rule applies to a configuration that is not final. */
enum Stuck:
  /** Reading a variable that is not in the store (rules Var, S-Var, M-Load). */
  case Unbound(x: String)

  /** n / 0 (rules Div, S-Div, M-Div). */
  case DivisionByZero

  /** The machine needs more values than there are on the stack. */
  case StackUnderflow(pc: Int, instr: Instr)

  /** The program counter is neither an instruction nor the end of the code. */
  case InvalidPc(pc: Int)

  def message: String = this match
    case Unbound(x)                => s"variable '$x' is read before being assigned"
    case DivisionByZero            => "division by zero"
    case StackUnderflow(pc, instr) => s"not enough values on the stack for ${instr.show} at $pc"
    case InvalidPc(pc)             => s"no instruction at $pc"

/** A run that got stuck, with the output it produced before. */
final case class Failure(reason: Stuck, out: Out)
