package scalino

import scala.annotation.tailrec

/** The reference interpreter: the big-step semantics, one case per rule. */
object Interpreter:
  import AExp.*, BExp.*, Com.*

  /** ⟨σ, ω⟩, what running a command produces. */
  final case class State(store: Store, out: Out)

  object State:
    /** ⟨∅, ε⟩ */
    val initial: State = State(Map.empty, Vector.empty)

  /** The meaning of a program: run it from the empty store, with nothing printed yet. */
  def run(program: Com): Either[Failure, State] = exec(program, State.initial)

  /** ⟨σ, a⟩ ⇓ n */
  def eval(a: AExp, store: Store): Either[Stuck, Value] = a match
    case Var(x) => store.get(x).toRight(Stuck.Unbound(x)) // Var
    case Num(n) => Right(n)                               // Num
    case Op(op, a1, a2) =>                                // Op
      for
        n1 <- eval(a1, store)
        n2 <- eval(a2, store)
      yield op(n1, n2)
    case Div(a1, a2) =>                                   // Div
      for
        n1 <- eval(a1, store)
        n2 <- eval(a2, store)
        _  <- Either.cond(n2 != 0, (), Stuck.DivisionByZero)
      yield n1 / n2 // BigInt division truncates towards zero, like ÷

  /** ⟨σ, b⟩ ⇓ true or ⟨σ, b⟩ ⇓ false */
  def eval(b: BExp, store: Store): Either[Stuck, Boolean] = b match
    case True  => Right(true)  // True
    case False => Right(false) // False
    case Lt(a1, a2) =>         // Lt-T, Lt-F
      for
        n1 <- eval(a1, store)
        n2 <- eval(a2, store)
      yield n1 < n2

  /** ⟨σ, ω, c⟩ ⇓ ⟨σ', ω'⟩ */
  def exec(c: Com, state: State): Either[Failure, State] =
    val State(store, out) = state
    c match
      case Skip             => Right(state)                                                     // Skip
      case Assign(x, a)     => eval(a, store).stuckWith(out).map(n => State(store.updated(x, n), out)) // Assgn
      case Sequence(c1, c2) => exec(c1, state).flatMap(exec(c2, _))                             // Seq
      case If(b, c1, c2) =>
        eval(b, store).stuckWith(out).flatMap {
          case true  => exec(c1, state) // If-T
          case false => exec(c2, state) // If-F
        }
      case While(b, body) => loop(b, body, state)
      case Print(a)       => eval(a, store).stuckWith(out).map(n => State(store, out :+ n))      // Print

  /** while b do c. While-T has the loop itself as a premise: it is unfolded into a loop,
    * so that long-running programs do not overflow the JVM stack.
    */
  @tailrec
  private def loop(b: BExp, body: Com, state: State): Either[Failure, State] =
    eval(b, state.store).stuckWith(state.out) match
      case Right(false) => Right(state)    // While-F
      case Right(true) =>                  // While-T
        exec(body, state) match
          case Right(next) => loop(b, body, next)
          case failure     => failure
      case Left(failure) => Left(failure)

  extension [A](result: Either[Stuck, A])
    /** A stuck expression makes the command stuck, after the output produced so far. */
    private def stuckWith(out: Out): Either[Failure, A] = result.left.map(Failure(_, out))
