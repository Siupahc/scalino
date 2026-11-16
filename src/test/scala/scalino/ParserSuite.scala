package scalino

import munit.Location

class ParserSuite extends ScalinoSuite:
  private def error(src: String)(using Location): StaticError = Parser.parse(src) match
    case Left(error)    => error
    case Right(program) => fail(s"expected a static error, got ${program.show}")

  test("* and / bind tighter than + and -"):
    assertEquals(runBoth(parse("print(1 + 2 * 3); print((1 + 2) * 3); print(7 - 6 / 3);")), out(7, 9, 5))

  test("the four operators associate to the left"):
    assertEquals(runBoth(parse("print(10 - 3 - 2); print(100 / 10 / 5);")), out(5, 2))

  test("if without else becomes if b then c else skip"):
    assertEquals(parse("var x = 0; if (x < 1) { x = 1; }").show, "x := 0; if x < 1 then x := 1 else skip")

  test("an empty block or file becomes skip"):
    assertEquals(parse("while (false) { }").show, "while false do skip")
    assertEquals(parse(""), Com.Skip)

  test("comments are ignored"):
    assertEquals(parse("// nothing\nprint(1); // one\n").show, "print 1")

  test("a variable must be declared before it is used"):
    assertEquals(error("print(x);"), StaticError(Pos(1, 7), "variable 'x' is not declared"))
    assertEquals(error("var x = x + 1;"), StaticError(Pos(1, 9), "variable 'x' is not declared"))

  test("a variable must be declared before it is assigned"):
    assertEquals(error("var y = 1;\nx = y;"), StaticError(Pos(2, 1), "variable 'x' is not declared"))

  test("a variable cannot be declared twice"):
    assertEquals(
      error("var x = 1;\nvar x = 2;"),
      StaticError(Pos(2, 5), "variable 'x' is already declared (at 1:5)"),
    )

  test("syntax errors point at the offending token"):
    assertEquals(error("var x = 1"), StaticError(Pos(1, 10), "expected ';' but found end of file"))
    assertEquals(error("while (1) { }"), StaticError(Pos(1, 9), "expected '<' but found ')'"))
    assertEquals(error("var x = 1 # 2;"), StaticError(Pos(1, 11), "unexpected character '#'"))
