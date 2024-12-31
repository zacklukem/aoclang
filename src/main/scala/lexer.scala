package aoclang

enum Tok:
  var span: Option[Span] = None

  infix def withSpan(span: Span): this.type =
    this.span = Some(span)
    this

  case Key(value: String)
  case Id(value: String)
  case Op(value: String)
  case Lit(value: LitValue)
  case Eof

private class Scanner(source: Source):
  private var start = 0
  private var pos = 0

  def peek: Option[Char] =
    if source.text.length <= pos then None else Some(source.text.charAt(pos))

  def next: Option[Char] =
    val c = peek
    if c.isDefined then pos += 1
    c

  def consume(predicate: Char => Boolean): Unit =
    while peek.exists(predicate) do next

  def lexeme: String =
    source.text.substring(start, pos)

  def close: Span =
    val start = this.start
    val end = pos
    this.start = pos
    source.span(start, end)

val KEYWORDS =
  Set(
    "def",
    "let",
    "if",
    "else",
    "match",
    "intrinsic",
    "(",
    ")",
    "{",
    "}",
    "[",
    "]",
    "=",
    ":",
    ",",
    ".",
    "=>"
  )

val OPERATOR_CHARS =
  Set('+', '-', '*', '/', '=', '!', '<', '>', '&', '|', '^', '~', '%', '?', ':', '.', ',')

extension (c: Char) private def isOperator: Boolean = OPERATOR_CHARS.contains(c)

private class LexerInner(source: Source):
  private val sc = Scanner(source)

  def next: Tok =
    sc.next match
      case Some('(' | ')' | '{' | '}' | '[' | ']' | ',' | '.') =>
        Tok.Key(sc.lexeme) withSpan sc.close

      case Some('\'') if sc.peek.exists(_.isUnicodeIdentifierStart) =>
        sc.next
        sc.consume(_.isUnicodeIdentifierPart)
        Tok.Lit(Sym(sc.lexeme.substring(1))) withSpan sc.close

      case Some(ch) if ch.isWhitespace =>
        sc.consume(_.isWhitespace)
        sc.close
        next

      case Some(ch) if ch.isOperator =>
        sc.consume(_.isOperator)
        val lexeme = sc.lexeme
        if KEYWORDS.contains(lexeme) then Tok.Key(lexeme) withSpan sc.close
        else Tok.Op(lexeme) withSpan sc.close

      case Some(ch) if ch.isUnicodeIdentifierStart =>
        sc.consume(_.isUnicodeIdentifierPart)
        val lexeme = sc.lexeme
        if KEYWORDS.contains(lexeme) then Tok.Key(lexeme) withSpan sc.close
        else Tok.Id(lexeme) withSpan sc.close

      case Some(ch) if ch.isDigit =>
        sc.consume(_.isDigit)
        if sc.peek.contains('.') then
          sc.next
          sc.consume(_.isDigit)
          Tok.Lit(sc.lexeme.toDouble) withSpan sc.close
        else Tok.Lit(sc.lexeme.toLong) withSpan sc.close

      case Some(ch) if ch == '"' =>
        sc.next
        sc.consume(_ != '"')
        val contents = sc.lexeme.substring(1)
        sc.next
        Tok.Lit(contents) withSpan sc.close

      case None => Tok.Eof

class Lexer(source: Source):
  private val inner = LexerInner(source)
  private var peeked: Option[Tok] = None

  def dump(): Unit =
    while peek != Tok.Eof do
      println(peek)
      next

  def peek: Tok =
    peeked match
      case Some(token) => token
      case None =>
        peeked = Some(inner.next)
        peeked.get

  def next: Tok =
    peeked match
      case Some(token) =>
        peeked = None
        token
      case None => inner.next
