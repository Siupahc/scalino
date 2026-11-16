package scalino

import scala.annotation.tailrec

/** The stack machine, one case per transition rule. */
object VM:
  /** ⟨pc, S, σ, ω⟩. The stack is a List, so the `n :: S` of the rules is `n :: stack`. */
  final case class Config(pc: Int, stack: List[Value], store: Store, out: Out)

  object Config:
    /** ⟨0, [], ∅, ε⟩ */
    val initial: Config = Config(0, Nil, Map.empty, Vector.empty)

  /** Runs `code` until it has finished (pc = |code|) or is stuck. */
  def run(code: Code, from: Config = Config.initial): Either[Failure, Config] =
    @tailrec
    def loop(config: Config): Either[Failure, Config] =
      if config.pc == code.length then Right(config)
      else step(code, config) match
        case Right(next)  => loop(next)
        case Left(reason) => Left(Failure(reason, config.out))

    loop(from)

  /** One transition →C, from a configuration where the machine has not finished. */
  def step(code: Code, config: Config): Either[Stuck, Config] =
    val Config(pc, stack, store, out) = config
    val next = config.copy(pc = pc + 1)
    code.lift(pc) match
      case None => Left(Stuck.InvalidPc(pc))
      case Some(instr) =>
        (instr, stack) match
          case (Instr.Push(n), s)                    => Right(next.copy(stack = n :: s))           // M-Push
          case (Instr.Load(x), s)                    =>                                            // M-Load
            store.get(x).map(n => next.copy(stack = n :: s)).toRight(Stuck.Unbound(x))
          case (Instr.Store(x), n :: s)              => Right(next.copy(stack = s, store = store.updated(x, n))) // M-Store
          case (Instr.Print, n :: s)                 => Right(next.copy(stack = s, out = out :+ n)) // M-Print
          case (Instr.Op(op), n2 :: n1 :: s)         => Right(next.copy(stack = op(n1, n2) :: s))  // M-Op
          case (Instr.Div, n2 :: n1 :: s) if n2 != 0 => Right(next.copy(stack = n1 / n2 :: s))      // M-Div
          case (Instr.Div, _ :: _ :: _)              => Left(Stuck.DivisionByZero)
          case (Instr.Lt, n2 :: n1 :: s) if n1 < n2  => Right(next.copy(stack = BigInt(1) :: s))   // M-LtT
          case (Instr.Lt, _ :: _ :: s)               => Right(next.copy(stack = BigInt(0) :: s))   // M-LtF
          case (Instr.Jmp(l), _)                     => Right(config.copy(pc = l))                 // M-Jmp
          case (Instr.Jmpz(l), n :: s) if n == 0     => Right(config.copy(pc = l, stack = s))      // M-JmpzT
          case (Instr.Jmpz(_), _ :: s)               => Right(next.copy(stack = s))                // M-JmpzF
          case _                                     => Left(Stuck.StackUnderflow(pc, instr))
