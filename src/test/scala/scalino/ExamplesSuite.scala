package scalino

import java.nio.file.{Files, Path}
import munit.Location

/** The example programs, end to end. */
class ExamplesSuite extends ScalinoSuite:
  import Instr.*

  private def example(name: String)(using Location): Com =
    parse(Files.readString(Path.of("examples", name)))

  test("Main.scalino is translated to abstract syntax"):
    assertEquals(
      example("Main.scalino").show,
      "i := 1; fact := 1; while i < 8 do " +
        "(fact := fact * i; if fact < 100 then print fact else print fact / 10; i := i + 1)",
    )

  test("Main.scalino prints 1, 2, 6, 24, 12, 72, 504"):
    assertEquals(runBoth(example("Main.scalino")), out(1, 2, 6, 24, 12, 72, 504))

  test("the squares program is translated to abstract syntax"):
    assertEquals(example("Squares.scalino").show, "i := 0; while i < 3 do (print i * i; i := i + 1)")

  test("the squares program outputs 0 · 1 · 4"):
    assertEquals(runBoth(example("Squares.scalino")), out(0, 1, 4))

  test("print 2 × x compiles to PUSH 2, LOAD x, MUL, PRINT"):
    val program = Com.Print(AExp.Op(ArithOp.Mul, AExp.Num(2), AExp.Var("x")))
    assertEquals(Compiler.compile(program), Vector(Push(2), Load("x"), Op(ArithOp.Mul), Print))

  test("the machine runs print 2 × x from σ = [x ↦ 3] step by step"):
    val code  = Vector(Push(2), Load("x"), Op(ArithOp.Mul), Print)
    val σ     = Map("x" -> BigInt(3))
    val start = VM.Config(0, Nil, σ, out())
    val trace = List.iterate(start, 5)(VM.step(code, _).orFail)
    assertEquals(
      trace,
      List(
        VM.Config(0, Nil, σ, out()),
        VM.Config(1, List[Value](2), σ, out()),    // M-Push
        VM.Config(2, List[Value](3, 2), σ, out()), // M-Load
        VM.Config(3, List[Value](6), σ, out()),    // M-Op
        VM.Config(4, Nil, σ, out(6)),              // M-Print
      ),
    )
    // pc = 4 = |C|: the machine has finished.
    assertEquals(VM.run(code, from = start), Right(trace.last))

  test("the squares program compiles to the listed code"):
    assertEquals(
      Compiler.compile(example("Squares.scalino")),
      Vector(
        Push(0), Store("i"),                              // 0-1
        Load("i"), Push(3), Lt, Jmpz(15),                 // 2-5   the test
        Load("i"), Load("i"), Op(ArithOp.Mul), Print,     // 6-9   the body
        Load("i"), Push(1), Op(ArithOp.Add), Store("i"),  // 10-13
        Jmp(2),                                           // 14
      ),
    )

  test("Main.scalinobc is plain text that reads back to the same code"):
    val code = Compiler.compile(example("Main.scalino"))
    val text = Bytecode.write(code)
    assertEquals(text.linesIterator.size, code.length)
    assertEquals(Bytecode.read(text), Right(code))
