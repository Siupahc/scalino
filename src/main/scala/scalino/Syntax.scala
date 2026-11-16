package scalino

// Abstract syntax of Scalino.

/** The ⊕ of the rules: the arithmetic operations that are always defined. */
enum ArithOp(val symbol: String):
  case Add extends ArithOp("+")
  case Sub extends ArithOp("-")
  case Mul extends ArithOp("*")

  /** The corresponding operation on integers. */
  def apply(n: Value, m: Value): Value = this match
    case Add => n + m
    case Sub => n - m
    case Mul => n * m

/** a ∈ Aexp. Division is kept apart from ⊕ because its rules have a side condition (m ≠ 0). */
enum AExp:
  case Var(x: String)
  case Num(n: Value)
  case Op(op: ArithOp, a1: AExp, a2: AExp)
  case Div(a1: AExp, a2: AExp)

  /** Abstract notation, with only the parentheses needed (operators associate to the left). */
  def show: String = this match
    case Var(x)         => x
    case Num(n)         => n.toString
    case Op(op, a1, a2) => s"${a1.showIn(precedence)} ${op.symbol} ${a2.showIn(precedence + 1)}"
    case Div(a1, a2)    => s"${a1.showIn(precedence)} / ${a2.showIn(precedence + 1)}"

  private def precedence: Int = this match
    case Op(ArithOp.Add | ArithOp.Sub, _, _) => 1
    case Op(ArithOp.Mul, _, _) | Div(_, _)   => 2
    case Var(_) | Num(_)                     => 3

  private def showIn(context: Int): String =
    if precedence < context then s"($show)" else show

/** b ∈ Bexp. */
enum BExp:
  case True, False
  case Lt(a1: AExp, a2: AExp)

  def show: String = this match
    case True       => "true"
    case False      => "false"
    case Lt(a1, a2) => s"${a1.show} < ${a2.show}"

/** c ∈ Com. A program is a command, run from the empty store. */
enum Com:
  case Skip
  case Assign(x: String, a: AExp)
  case Sequence(c1: Com, c2: Com)
  case If(b: BExp, c1: Com, c2: Com)
  case While(b: BExp, c: Com)
  case Print(a: AExp)

  /** Abstract notation, e.g. `i := 0; while i < 3 do (print i * i; i := i + 1)`. */
  def show: String = this match
    case Skip             => "skip"
    case Assign(x, a)     => s"$x := ${a.show}"
    case Sequence(c1, c2) => s"${c1.showNested}; ${c2.show}"
    case If(b, c1, c2)    => s"if ${b.show} then ${c1.showNested} else ${c2.showNested}"
    case While(b, c)      => s"while ${b.show} do ${c.showNested}"
    case Print(a)         => s"print ${a.show}"

  /** `;` binds the loosest, so a sequence inside another command needs parentheses. */
  private def showNested: String = this match
    case Sequence(_, _) => s"($show)"
    case _              => show
