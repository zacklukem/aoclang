package aoclang

import scala.collection.mutable

class Parser(source: Source):
  private val lx = Lexer(source)

  private def expectEq(actual: Tok, expected: Tok): Unit =
    if actual != expected then
      actual.span.get.printLineColumn("error")
      throw new Exception(s"Expected $expected got $actual")

  def parseTopLevel: List[Decl] =
    val decls = mutable.ListBuffer.empty[Decl]
    while lx.peek != Tok.Eof do decls += parseDecl
    decls.toList

  private def parseDeclArgList(terminator: Tok = Tok.Key(")")): List[Pat] =
    val args = mutable.ListBuffer.empty[Pat]
    while lx.peek != terminator do
      val name = parsePat
      args += name
      if lx.peek == Tok.Key(",") then lx.next
      else expectEq(lx.peek, terminator)
    args.toList

  private def parseDecl =
    lx.peek match
      case Tok.Key("def") =>
        lx.next
        val name = lx.next.asInstanceOf[Tok.Id | Tok.Op]
        if lx.peek == Tok.Key(".") then
          lx.next
          val protocol = lx.next.asInstanceOf[Tok.Id]

          expectEq(lx.next, Tok.Key("("))
          val args = parseDeclArgList()
          expectEq(lx.next, Tok.Key(")"))
          expectEq(lx.next, Tok.Key("="))
          val body: Expr = parseSingleExpr

          Decl.AbsoluteDef(List(name.asInstanceOf[Tok.Id], protocol), args, body)
        else
          expectEq(lx.next, Tok.Key("("))
          val args = parseDeclArgList()
          expectEq(lx.next, Tok.Key(")"))
          expectEq(lx.next, Tok.Key("="))
          val body: Expr = parseSingleExpr

          Decl.Def(name, args, body)
      case tok => emit(tok.span.get, s"Unexpected: $tok")

  private def parsePatList(terminator: String = ")"): List[Pat] =
    val args = mutable.ListBuffer.empty[Pat]
    while lx.peek != Tok.Key(terminator) do
      args += parsePat
      if lx.peek == Tok.Key(",") then lx.next
      else expectEq(lx.peek, Tok.Key(terminator))
    args.toList

  private def parsePat =
    // TODO: Implement operator precedence
    var lhs = parsePatPrimary
    while lx.peek == Tok.Op("::") do
      lx.next
      val rhs = parsePatPrimary
      lhs = Pat.Cons(lhs, rhs)
    lhs

  private def parsePatPrimary: Pat =
    val pat = lx.peek match
      case Tok.Key("(") =>
        val l = lx.next
        val pats = parsePatList()
        val r = lx.next
        expectEq(r, Tok.Key(")"))
        if pats.length == 1 then pats.head
        else Pat.Tuple(l.asInstanceOf, pats, r.asInstanceOf)
      case Tok.Key("[") =>
        val l = lx.next
        val pats = parsePatList("]")
        val r = lx.next
        expectEq(r, Tok.Key("]"))
        Pat.ListLit(l.asInstanceOf, pats, r.asInstanceOf)
      case lit: Tok.Lit =>
        lx.next
        Pat.Lit(lit)
      case tok @ Tok.Id(_) =>
        lx.next
        Pat.Bind(tok)

    if lx.peek == Tok.Key(":") then
      lx.next
      Pat.TypeAssert(pat, lx.next.asInstanceOf[Tok.Id])
    else pat

  private def parseBlockExpr: Expr =
    val l = lx.next
    expectEq(l, Tok.Key("{"))

    def parseCont: Option[Expr] =
      lx.peek match
        case Tok.Key("}") => None
        case _ =>
          val exprFn = parseInBlockExpr
          val succ = parseCont
          Some(succ |> exprFn)

    val expr = parseCont
    val r = lx.next
    expectEq(r, Tok.Key("}"))
    Expr.Block(l.asInstanceOf[Tok.Key], expr, r.asInstanceOf[Tok.Key])

  private def parseInBlockExpr: Option[Expr] => Expr =
    lx.peek match
      case Tok.Key("let") =>
        lx.next
        val pat = parsePat
        expectEq(lx.next, Tok.Key("="))
        val rhs = parseSingleExpr

        body => Expr.Let(pat, rhs, body)

      case _ =>
        val expr = parseSingleExpr
        {
          case Some(body) => Expr.Discard(expr, body)
          case None       => expr
        }

  def parseSingleExpr: Expr =
    lx.peek match
      case Tok.Key("{") => parseBlockExpr

      case Tok.Op("/") =>
        val l = lx.next
        val args = parseDeclArgList(Tok.Op("/"))
        val r = lx.next
        expectEq(r, Tok.Op("/"))
        val body = parseSingleExpr
        Expr.Lambda(l.asInstanceOf, args, r.asInstanceOf, body)

      case Tok.Key("match") =>
        val l = lx.next
        val expr = parseSingleExpr
        expectEq(lx.next, Tok.Key("{"))
        val cases = mutable.ListBuffer.empty[MatchCase]

        while lx.peek != Tok.Key("}") do
          val pat = parsePat
          expectEq(lx.next, Tok.Key("=>"))
          val body = parseSingleExpr
          cases += MatchCase(pat, body)
          if lx.peek == Tok.Key(",") then lx.next
          else expectEq(lx.peek, Tok.Key("}"))

        val r = lx.next
        expectEq(r, Tok.Key("}"))
        Expr.Match(expr, l.asInstanceOf, cases.toList, r.asInstanceOf)

      case Tok.Key("if") =>
        lx.next
        val cond = parseSingleExpr
        val thenExpr = parseSingleExpr
        val elseExpr = lx.peek match
          case Tok.Key("else") =>
            lx.next
            Some(parseSingleExpr)
          case _ => None

        Expr.If(cond, thenExpr, elseExpr)

      case _ => parsePipe

  private def parsePipe =
    var lhs = parseBinop(0)
    while lx.peek == Tok.Key("|>") do
      lx.next
      val Expr.App(fn, args) = parsePrimaryExpr
      lhs = Expr.App(fn, lhs :: args)
    lhs

  private def parseBinop(min: Int): Expr =
    // TODO: Implement operator precedence
    var lhs = parseUnop
    while isOp(lx.peek, min) do
      val op = lx.next.asInstanceOf[Tok.Op]
      val min = prec(op) + assoc(op)
      val rhs = parseBinop(min)
      lhs = Expr.App(Expr.Var(op), List(lhs, rhs))
    lhs

  private def parseUnop =
    if lx.peek.isInstanceOf[Tok.Op] then
      val op = lx.next.asInstanceOf[Tok.Op]
      val rhs = parsePrimaryExpr
      Expr.App(Expr.Var(op), List(rhs))
    else parsePrimaryExpr

  private def parseArgList(terminator: String = ")") =
    val args = mutable.ListBuffer.empty[Expr]
    while lx.peek != Tok.Key(terminator) do
      args += parseSingleExpr
      if lx.peek == Tok.Key(",") then lx.next
      else expectEq(lx.peek, Tok.Key(terminator))
    args.toList

  private def parsePrimaryExpr =
    var expr = lx.peek match
      case Tok.Key("[") =>
        val l = lx.next
        val exprs = parseArgList("]")
        val r = lx.next
        expectEq(r, Tok.Key("]"))
        Expr.ListLit(l.asInstanceOf, exprs, r.asInstanceOf)

      case tok @ Tok.Key("(") =>
        val l = lx.next
        val exprs = parseArgList()
        val r = lx.next
        expectEq(r, Tok.Key(")"))
        if exprs.length == 1 then exprs.head
        else Expr.Tuple(l.asInstanceOf, exprs, r.asInstanceOf)

      case id @ Tok.Id(_) =>
        lx.next
        Expr.Var(id)

      case lit @ Tok.Lit(_) =>
        lx.next
        Expr.Lit(lit)

      case tok => emit(tok.span.get, s"Unexpected: $tok")

    while (lx.peek == Tok.Key("(") && !lx.peek.span.get.isAfterNewline) || lx.peek == Tok.Key(".")
    do
      lx.peek match
        case Tok.Key("(") =>
          lx.next
          val args = parseArgList()
          expr = Expr.App(expr, args)
          expectEq(lx.next, Tok.Key(")"))
        case Tok.Key(".") =>
          lx.next
          lx.next match
            case field: Tok.Id =>
              expr = Expr.Field(expr, field)
            case num @ Tok.Lit(n: Long) =>
              expr = Expr.TupleField(expr, num)

    expr

def isOp(op: Tok, min: Int) =
  op match
    case op: Tok.Op => prec(op) >= min
    case _          => false

def prec(op: Tok.Op) = op.value match
  case "!=" | "=="             => 1
  case "<" | ">" | "<=" | ">=" => 2
  case "++" | "--"             => 3
  case "|"                     => 4
  case "^"                     => 5
  case "&"                     => 6
  case "<<" | ">>"             => 7
  case "+" | "-"               => 8
  case "*" | "/" | "/%" | "%"  => 9
  case "**"                    => 10
  case _                       => 0

def assoc(op: Tok.Op) = op.value match
  case "::" => 0
  case _    => 1
