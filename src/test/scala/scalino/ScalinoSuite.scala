package scalino

import munit.Location

/** Helpers shared by the test suites. */
trait ScalinoSuite extends munit.FunSuite:
  def out(ns: Int*): Out = ns.map(BigInt(_)).toVector

  extension [E, A](result: Either[E, A])
    def orFail(using Location): A = result.fold(error => fail(s"unexpected failure: $error"), identity)

  def parse(src: String)(using Location): Com = Parser.parse(src).orFail

  /** Runs a program with the interpreter and on the VM, checks the correctness theorem on it,
    * ⟨∅, ε, c⟩ ⇓ ⟨σ, ω⟩ implies ⟨0, [], ∅, ε⟩ →* ⟨|C0(c)|, [], σ, ω⟩, and returns ω.
    */
  def runBoth(program: Com)(using Location): Out =
    val code = Compiler.compile(program)
    val Interpreter.State(store, out) = Interpreter.run(program).orFail
    assertEquals(VM.run(code), Right(VM.Config(code.length, Nil, store, out)))
    out

  /** Checks that a program gets stuck in the same way, after the same output, with the
    * interpreter and on the VM.
    */
  def stuckBoth(program: Com)(using Location): Failure =
    (Interpreter.run(program), VM.run(Compiler.compile(program))) match
      case (Left(viaInterpreter), Left(viaVM)) =>
        assertEquals(viaVM, viaInterpreter)
        viaInterpreter
      case results => fail(s"expected both to be stuck, got $results")
