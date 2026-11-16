package scalino

/** Corner cases of the semantics, checked on the interpreter and on the VM. */
class SemanticsSuite extends ScalinoSuite:
  test("division truncates towards zero"):
    assertEquals(runBoth(parse("print((0 - 7) / 2); print(7 / (0 - 2));")), out(-3, -3))

  test("integers are unbounded"):
    val program = parse("var x = 1; var i = 0; while (i < 100) { x = x * 2; i = i + 1; } print(x);")
    assertEquals(runBoth(program), Vector(BigInt(2).pow(100)))

  test("a long loop does not overflow the JVM stack"):
    assertEquals(runBoth(parse("var i = 0; while (i < 100000) { i = i + 1; } print(i);")), out(100000))

  test("dividing by zero is stuck, after what was printed before"):
    assertEquals(
      stuckBoth(parse("print(1); print(1 / 0); print(2);")),
      Failure(Stuck.DivisionByZero, out(1)),
    )

  test("declared is not assigned: reading x is stuck at runtime"):
    assertEquals(
      stuckBoth(parse("if (false) { var x = 1; } print(x);")),
      Failure(Stuck.Unbound("x"), out()),
    )
