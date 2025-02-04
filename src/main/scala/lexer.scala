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
    "=>",
    "|>"
  )

val OPERATOR_CHARS =
  Set('+', '-', '*', '/', '=', '!', '<', '>', '&', '|', '^', '~', '%', '?', ':', '.', ',', '@')

def convertEscapeChar(c: Char) =
  c match
    case 'n' => '\n'
    case 'r' => '\r'
    case 't' => '\t'
    case 'b' => '\b'
    case 'f' => '\f'
    case c   => c

def cleanEscape(s: String) =
  val sb = new StringBuilder
  var i = 0
  while i < s.length do
    val c = s.charAt(i)
    if c == '\\' then
      i += 1
      val c2 = s.charAt(i)
      sb.append(convertEscapeChar(c2))
    else sb.append(c)
    i += 1
  sb.toString

extension (c: Char)
  private def isOperator: Boolean = OPERATOR_CHARS.contains(c)
  private def isIdentStart: Boolean = c.isUnicodeIdentifierStart || c == '_'

private class LexerInner(source: Source):
  private val sc = Scanner(source)

  def next: Tok =
    sc.next match
      case Some('#') =>
        sc.consume(_ != '\n')
        sc.close
        next

      case Some('/') if sc.peek.contains('/') =>
        Tok.Op(sc.lexeme) withSpan sc.close

      case Some('(' | ')' | '{' | '}' | '[' | ']' | ',' | '.') =>
        Tok.Key(sc.lexeme) withSpan sc.close

      case Some('`') if sc.peek.contains('\\') =>
        sc.next
        val v = convertEscapeChar(sc.next.get)
        Tok.Lit(v.toLong) withSpan sc.close

      case Some('`') =>
        val v = sc.next.get
        Tok.Lit(v.toLong) withSpan sc.close

      case Some('\'') if sc.peek.exists(_.isIdentStart) =>
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

      case Some(ch) if ch.isIdentStart =>
        sc.consume(_.isUnicodeIdentifierPart)
        sc.consume(_ == '\'')
        val lexeme = sc.lexeme
        lexeme match
          case "true"                              => Tok.Lit(true) withSpan sc.close
          case "false"                             => Tok.Lit(false) withSpan sc.close
          case lexeme if KEYWORDS.contains(lexeme) => Tok.Key(lexeme) withSpan sc.close
          case _                                   => Tok.Id(lexeme) withSpan sc.close

      case Some(ch) if ch.isDigit =>
        sc.consume(_.isDigit)
        if sc.peek.contains('.') then
          sc.next
          sc.consume(_.isDigit)
          Tok.Lit(sc.lexeme.toDouble) withSpan sc.close
        else Tok.Lit(sc.lexeme.toLong) withSpan sc.close

      case Some(ch) if ch == '"' =>
        sc.next
        while sc.peek.exists(_ != '"') do
          if sc.peek.contains('\\') then sc.next
          sc.next
        val contents = cleanEscape(sc.lexeme.substring(1))
        assert(sc.next.contains('"'))
        Tok.Lit(contents) withSpan sc.close

      case Some(ch) => throw RuntimeException(s"unexpected character: $ch")

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
