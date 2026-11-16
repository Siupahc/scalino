package scalino

import scala.annotation.tailrec
import scala.util.control.NoStackTrace

/** Statements of the concrete syntax, before the translation to abstract syntax. */
enum Stmt:
  case VarDecl(x: String, a: AExp)                                     // var x = a;
  case Assign(x: String, a: AExp)                                      // x = a;
  case Print(a: AExp)                                                  // print(a);
  case If(b: BExp, thenPart: List[Stmt], elsePart: Option[List[Stmt]]) // if (b) { s1 } else { s2 }
  case While(b: BExp, body: List[Stmt])                                // while (b) { s }

/** From a `.scalino` file to abstract syntax.
  *
  * Grammar, where `*` binds tighter than `+`, and all four operators associate to the left:
  * {{{
  * program ::= stmt*
  * stmt    ::= 'var' x '=' aexp ';' | x '=' aexp ';' | 'print' '(' aexp ')' ';'
  *           | 'if' '(' bexp ')' block ('else' block)? | 'while' '(' bexp ')' block
  * block   ::= '{' stmt* '}'
  * bexp    ::= 'true' | 'false' | aexp '<' aexp
  * aexp    ::= term (('+' | '-') term)*
  * term    ::= factor (('*' | '/') factor)*
  * factor  ::= n | x | '(' aexp ')'
  * }}}
  */
object Parser:
  def parse(src: String): Either[StaticError, Com] =
    parseStatements(src).map(desugar)

  /** Parses a file into concrete statements, checking that variables are declared with `var`
    * before being used or assigned (in the order of the file), and only once.
    */
  def parseStatements(src: String): Either[StaticError, List[Stmt]] =
    Lexer.tokenize(src).flatMap: tokens =>
      try Right(RecursiveDescent(tokens).program())
      catch case Abort(error) => Left(error)

  /** The translation from concrete to abstract syntax. */
  def desugar(stmts: List[Stmt]): Com = stmts match
    case Nil         => Com.Skip                                 // empty sequence ⇝ skip
    case st :: Nil   => desugar(st)
    case st :: rest  => Com.Sequence(desugar(st), desugar(rest)) // st1 st2 … stn ⇝ c1; (c2; (…; cn))

  def desugar(stmt: Stmt): Com = stmt match
    case Stmt.VarDecl(x, a)       => Com.Assign(x, a)
    case Stmt.Assign(x, a)        => Com.Assign(x, a)
    case Stmt.Print(a)            => Com.Print(a)
    case Stmt.If(b, s1, Some(s2)) => Com.If(b, desugar(s1), desugar(s2))
    case Stmt.If(b, s, None)      => Com.If(b, desugar(s), Com.Skip)
    case Stmt.While(b, s)         => Com.While(b, desugar(s))

  /** Carries a static error out of the recursive descent. */
  private final case class Abort(error: StaticError) extends Exception with NoStackTrace

  /** Parser for one file. Its state is mutable, but lives only during one call to [[parseStatements]]. */
  private final class RecursiveDescent(tokens: Vector[Token]):
    import TokenKind.*

    private var index = 0

    /** The variables declared so far, with where they were declared. */
    private var declared = Map.empty[String, Pos]

    def program(): List[Stmt] = statements(until = EOF)

    // Statements

    private def statements(until: TokenKind): List[Stmt] =
      val stmts = List.newBuilder[Stmt]
      while peek.kind != until && peek.kind != EOF do stmts += statement()
      stmts.result()

    private def block(): List[Stmt] = between(LBrace, RBrace)(statements(until = RBrace))

    private def statement(): Stmt =
      val start = next()
      start.kind match
        case Var =>
          val (x, pos) = identifier()
          declared.get(x).foreach(first => abort(pos, s"variable '$x' is already declared (at $first)"))
          expect(Equals)
          val a = aexp()
          expect(Semicolon)
          declared += x -> pos
          Stmt.VarDecl(x, a)
        case Ident(x) =>
          checkDeclared(x, start.pos)
          expect(Equals)
          val a = aexp()
          expect(Semicolon)
          Stmt.Assign(x, a)
        case Print =>
          val a = between(LParen, RParen)(aexp())
          expect(Semicolon)
          Stmt.Print(a)
        case If =>
          val b        = between(LParen, RParen)(bexp())
          val thenPart = block()
          val elsePart = Option.when(accept(Else))(block())
          Stmt.If(b, thenPart, elsePart)
        case While =>
          val b = between(LParen, RParen)(bexp())
          Stmt.While(b, block())
        case other =>
          abort(start.pos, s"expected a statement but found ${other.show}")

    // Expressions

    private def bexp(): BExp = peek.kind match
      case True  => next(); BExp.True
      case False => next(); BExp.False
      case _ =>
        val a1 = aexp()
        expect(Less)
        BExp.Lt(a1, aexp())

    private def aexp(): AExp = leftAssoc(term) {
      case Plus  => AExp.Op(ArithOp.Add, _, _)
      case Minus => AExp.Op(ArithOp.Sub, _, _)
    }

    private def term(): AExp = leftAssoc(factor) {
      case Times  => AExp.Op(ArithOp.Mul, _, _)
      case Divide => AExp.Div(_, _)
    }

    private def factor(): AExp =
      val token = next()
      token.kind match
        case IntLit(n) => AExp.Num(n)
        case Ident(x) =>
          checkDeclared(x, token.pos)
          AExp.Var(x)
        case LParen =>
          val a = aexp()
          expect(RParen)
          a
        case other =>
          abort(token.pos, s"expected an expression but found ${other.show}")

    /** operand (operator operand)*, combined from the left. */
    private def leftAssoc(operand: () => AExp)(
        operator: PartialFunction[TokenKind, (AExp, AExp) => AExp],
    ): AExp =
      @tailrec
      def loop(lhs: AExp): AExp = operator.lift(peek.kind) match
        case Some(combine) =>
          next()
          loop(combine(lhs, operand()))
        case None => lhs

      loop(operand())

    // Helpers

    private def peek: Token = tokens(index)

    private def next(): Token =
      val token = peek
      if token.kind != EOF then index += 1
      token

    private def accept(kind: TokenKind): Boolean =
      val found = peek.kind == kind
      if found then next()
      found

    private def expect(kind: TokenKind): Token =
      if peek.kind == kind then next()
      else abort(peek.pos, s"expected ${kind.show} but found ${peek.kind.show}")

    private def between[A](open: TokenKind, close: TokenKind)(body: => A): A =
      expect(open)
      val result = body
      expect(close)
      result

    private def identifier(): (String, Pos) =
      val token = next()
      token.kind match
        case Ident(x) => (x, token.pos)
        case other    => abort(token.pos, s"expected a variable name but found ${other.show}")

    private def checkDeclared(x: String, pos: Pos): Unit =
      if !declared.contains(x) then abort(pos, s"variable '$x' is not declared")

    private def abort(pos: Pos, message: String): Nothing =
      throw Abort(StaticError(pos, message))
