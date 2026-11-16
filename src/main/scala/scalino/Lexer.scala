package scalino

import scala.annotation.tailrec

/** A position in a source file, starting at 1:1. */
final case class Pos(line: Int, col: Int):
  override def toString: String = s"$line:$col"

/** An error found before running anything: lexing, parsing, declarations, bytecode format. */
final case class StaticError(pos: Pos, message: String):
  override def toString: String = s"$pos: $message"

enum TokenKind:
  case Ident(name: String)
  case IntLit(value: Value)
  case Var, If, Else, While, Print, True, False
  case Plus, Minus, Times, Divide, Less, Equals, LParen, RParen, LBrace, RBrace, Semicolon
  case EOF

  /** How the token is named in error messages. */
  def show: String = this match
    case Ident(x)  => s"variable name '$x'"
    case IntLit(n) => s"number $n"
    case EOF       => "end of file"
    case kind      => s"'${Lexer.spelling(kind)}'"

final case class Token(kind: TokenKind, pos: Pos)

object Lexer:
  import TokenKind.*

  val keywords: Map[String, TokenKind] = Map(
    "var" -> Var, "if" -> If, "else" -> Else, "while" -> While,
    "print" -> Print, "true" -> True, "false" -> False,
  )

  val symbols: Map[Char, TokenKind] = Map(
    '+' -> Plus, '-' -> Minus, '*' -> Times, '/' -> Divide, '<' -> Less, '=' -> Equals,
    '(' -> LParen, ')' -> RParen, '{' -> LBrace, '}' -> RBrace, ';' -> Semicolon,
  )

  /** How keywords and symbols are written. */
  val spelling: Map[TokenKind, String] =
    keywords.map(_.swap) ++ symbols.map((c, kind) => kind -> c.toString)

  def isIdentStart(c: Char): Boolean = c.isLetter || c == '_'
  def isIdentPart(c: Char): Boolean  = c.isLetterOrDigit || c == '_'

  /** Whether `s` can be used as a variable name. */
  def isIdentifier(s: String): Boolean =
    s.nonEmpty && isIdentStart(s.head) && s.tail.forall(isIdentPart) && !keywords.contains(s)

  /** Splits a source file into tokens, ending with EOF. Comments start with `//`. */
  def tokenize(src: String): Either[StaticError, Vector[Token]] =
    @tailrec
    def loop(i: Int, line: Int, col: Int, tokens: Vector[Token]): Either[StaticError, Vector[Token]] =
      val pos = Pos(line, col)
      if i == src.length then Right(tokens :+ Token(EOF, pos))
      else if src(i) == '\n' then loop(i + 1, line + 1, 1, tokens)
      else lexeme(src, i) match
        case Right((length, kind)) => loop(i + length, line, col + length, tokens ++ kind.map(Token(_, pos)))
        case Left(message)         => Left(StaticError(pos, message))

    loop(0, 1, 1, Vector.empty)

  /** The lexeme starting at index i: its length, and its token unless it is blank or a comment. */
  private def lexeme(src: String, i: Int): Either[String, (Int, Option[TokenKind])] =
    def span(p: Char => Boolean): String = src.substring(i, i + src.segmentLength(p, i))

    src(i) match
      case c if c.isWhitespace            => Right((1, None))
      case '/' if src.startsWith("//", i) => Right((span(_ != '\n').length, None))
      case c if c.isDigit =>
        val digits = span(_.isDigit)
        Right((digits.length, Some(IntLit(BigInt(digits)))))
      case c if isIdentStart(c) =>
        val word = span(isIdentPart)
        Right((word.length, Some(keywords.getOrElse(word, Ident(word)))))
      case c =>
        symbols.get(c).map(kind => (1, Some(kind))).toRight(s"unexpected character '$c'")
