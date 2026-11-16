package scalino

/** The `.scalinobc` format, and the machine on code that the compiler does not produce. */
class BytecodeSuite extends ScalinoSuite:
  import Instr.*

  test("the text format has one instruction per line"):
    assertEquals(Bytecode.write(Vector(Push(2), Load("x"), Op(ArithOp.Mul), Print)), "PUSH 2\nLOAD x\nMUL\nPRINT\n")

  test("every instruction reads back to itself"):
    val code = Vector(
      Push(-3), Load("x"), Store("x"), Op(ArithOp.Add), Op(ArithOp.Sub), Op(ArithOp.Mul),
      Div, Lt, Print, Jmp(0), Jmpz(9),
    )
    assertEquals(Bytecode.read(Bytecode.write(code)), Right(code))

  test("an empty file is the empty code"):
    assertEquals(Bytecode.read(""), Right(Vector.empty))

  test("format errors give the line"):
    assertEquals(Bytecode.read("PUSH 1\nPUSH x\n"), Left(StaticError(Pos(2, 1), "invalid instruction 'PUSH x'")))
    assertEquals(Bytecode.read("JMP -1"), Left(StaticError(Pos(1, 1), "invalid instruction 'JMP -1'")))

  test("the machine is stuck when the stack is too short"):
    assertEquals(VM.run(Vector(Push(1), Op(ArithOp.Add))), Left(Failure(Stuck.StackUnderflow(1, Op(ArithOp.Add)), out())))

  test("the machine is stuck when jumping outside the code"):
    assertEquals(VM.run(Vector(Jmp(5), Print)), Left(Failure(Stuck.InvalidPc(5), out())))
