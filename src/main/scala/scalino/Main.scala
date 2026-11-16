package scalino

import java.nio.file.{Files, NoSuchFileException, Path}
import scala.util.Try

@main def scalino(args: String*): Unit = sys.exit(Cli.run(args))

/** The `scalino` command. The values printed by the program go to stdout, one per
  * line, and errors to stderr. Exit code: 0 if all went well, 1 if nothing could be run (bad
  * arguments, missing file, static error), 2 if the program got stuck.
  */
object Cli:
  val usage: String =
    """usage: scalino FILE.scalino            run the program with the interpreter
      |       scalino -compile FILE.scalino   compile it, writing FILE.scalinobc
      |       scalino -vm FILE.scalinobc      run compiled bytecode on the VM""".stripMargin

  enum CliError:
    case Usage
    case Io(file: Path, reason: String)
    case Static(file: Path, error: StaticError)
    case Runtime(failure: Failure)

  def run(args: Seq[String]): Int =
    val outcome: Either[CliError, Out] = args match
      case Seq("-compile", file)              => compile(Path.of(file)).map(_ => Vector.empty)
      case Seq("-vm", file)                   => runBytecode(Path.of(file))
      case Seq(file) if !file.startsWith("-") => interpret(Path.of(file))
      case _                                  => Left(CliError.Usage)

    outcome match
      case Right(out) =>
        printOut(out)
        0
      case Left(error @ CliError.Runtime(failure)) =>
        printOut(failure.out)
        Console.err.println(describe(error))
        2
      case Left(error) =>
        Console.err.println(describe(error))
        1

  def interpret(file: Path): Either[CliError, Out] =
    for
      src     <- readFile(file)
      program <- Parser.parse(src).left.map(CliError.Static(file, _))
      result  <- Interpreter.run(program).left.map(CliError.Runtime(_))
    yield result.out

  /** Compiles `file` and returns the path of the bytecode written next to it. */
  def compile(file: Path): Either[CliError, Path] =
    val target = file.resolveSibling(file.getFileName.toString.stripSuffix(".scalino") + ".scalinobc")
    for
      src     <- readFile(file)
      program <- Parser.parse(src).left.map(CliError.Static(file, _))
      _       <- attempt(target)(Files.writeString(target, Bytecode.write(Compiler.compile(program))))
    yield target

  def runBytecode(file: Path): Either[CliError, Out] =
    for
      text   <- readFile(file)
      code   <- Bytecode.read(text).left.map(CliError.Static(file, _))
      result <- VM.run(code).left.map(CliError.Runtime(_))
    yield result.out

  private def printOut(out: Out): Unit = out.foreach(println)

  private def describe(error: CliError): String = error match
    case CliError.Usage               => usage
    case CliError.Io(file, reason)    => s"$file: error: $reason"
    case CliError.Static(file, error) => s"$file:${error.pos}: error: ${error.message}"
    case CliError.Runtime(failure)    => s"error: stuck: ${failure.reason.message}"

  private def readFile(file: Path): Either[CliError, String] = attempt(file)(Files.readString(file))

  private def attempt[A](file: Path)(action: => A): Either[CliError, A] =
    Try(action).toEither.left.map:
      case _: NoSuchFileException => CliError.Io(file, "no such file")
      case e                      => CliError.Io(file, Option(e.getMessage).getOrElse(e.toString))
