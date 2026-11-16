package scalino

/** The compilation function C.
  *
  * Jumps go to absolute indices, so the code of a command depends on the index p where it
  * starts, written Cp(c). Only the jump targets depend on p, not the length of the code.
  */
object Compiler:
  import AExp.*, BExp.*, Com.*

  /** C(a): code that pushes the value of a. */
  def compile(a: AExp): Code = a match
    case Num(n)         => Vector(Instr.Push(n))
    case Var(x)         => Vector(Instr.Load(x))
    case Op(op, a1, a2) => compile(a1) ++ compile(a2) :+ Instr.Op(op)
    case Div(a1, a2)    => compile(a1) ++ compile(a2) :+ Instr.Div

  /** C(b): code that pushes 1 if b holds, 0 otherwise. */
  def compile(b: BExp): Code = b match
    case True       => Vector(Instr.Push(1))
    case False      => Vector(Instr.Push(0))
    case Lt(a1, a2) => compile(a1) ++ compile(a2) :+ Instr.Lt

  /** C0(c): a whole program. */
  def compile(program: Com): Code = compile(program, 0)

  /** Cp(c): code for c, to be placed at index p. */
  def compile(c: Com, p: Int): Code = c match
    case Skip         => Vector.empty
    case Assign(x, a) => compile(a) :+ Instr.Store(x)
    case Print(a)     => compile(a) :+ Instr.Print
    case Sequence(c1, c2) =>
      val code1 = compile(c1, p)
      code1 ++ compile(c2, p + code1.length)
    case If(b, c1, c2) =>
      val test     = compile(b)
      val p1       = p + test.length + 1
      val thenCode = compile(c1, p1)
      val l2       = p1 + thenCode.length + 1
      val elseCode = compile(c2, l2)
      val l3       = l2 + elseCode.length
      test ++ Vector(Instr.Jmpz(l2)) ++ thenCode ++ Vector(Instr.Jmp(l3)) ++ elseCode
    case While(b, body) =>
      val test     = compile(b)
      val p1       = p + test.length + 1
      val bodyCode = compile(body, p1)
      val l        = p1 + bodyCode.length + 1
      test ++ Vector(Instr.Jmpz(l)) ++ bodyCode ++ Vector(Instr.Jmp(p))
