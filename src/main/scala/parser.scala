package aoclang

import scala.collection.mutable

class Parser(source: Source):
  private val lx = Lexer(source)

  def expectEq(actual: Tok, expected: Tok) =
    if actual != expected then
      actual.span.get.printLineColumn("error")
      throw new Exception(s"Expected $expected got $actual")

  def parseTopLevel =
    val decls = mutable.ListBuffer.empty[Decl]
    while lx.peek != Tok.Eof do decls += parseDecl
    decls.toList

  def parseDeclArgList =
    expectEq(lx.next, Tok.Key("("))
    val args = mutable.ListBuffer.empty[Pat]
    while lx.peek != Tok.Key(")") do
      val name = parsePat
      args += name
      if lx.peek == Tok.Key(",") then lx.next
      else expectEq(lx.peek, Tok.Key(")"))
    expectEq(lx.next, Tok.Key(")"))
    args.toList

  def parseDecl =
    lx.peek match
      case Tok.Key("def") =>
        lx.next
        val name = lx.next.asInstanceOf[Tok.Id | Tok.Op]
        val args = parseDeclArgList
        expectEq(lx.next, Tok.Key("="))
        val body: Expr | Intrinsic =
          if lx.peek == Tok.Key("intrinsic") then Intrinsic(lx.next.span.get)
          else parseSingleExpr

        Decl.Def(name, args, body)

  def parsePatList: List[Pat] =
    val args = mutable.ListBuffer.empty[Pat]
    while lx.peek != Tok.Key(")") do
      args += parsePat
      if lx.peek == Tok.Key(",") then lx.next
      else expectEq(lx.peek, Tok.Key(")"))
    args.toList

  def parsePat: Pat =
    lx.peek match
      case Tok.Key("(") =>
        val l = lx.next
        val pats = parsePatList
        val r = lx.next
        expectEq(r, Tok.Key(")"))
        if pats.length == 1 then pats.head
        else Pat.Tuple(l.asInstanceOf, pats, r.asInstanceOf)
      case lit: Tok.Lit =>
        lx.next
        Pat.Lit(lit)
      case tok @ Tok.Id(_) =>
        lx.next
        Pat.Bind(tok)

  def parseBlockExpr: Expr =
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

  def parseInBlockExpr: Option[Expr] => Expr =
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

      case _ => parseBinop

  def parseBinop =
    // TODO: Implement operator precedence
    var lhs = parseUnop
    while lx.peek.isInstanceOf[Tok.Op] do
      val op = lx.next.asInstanceOf[Tok.Op]
      val rhs = parseUnop
      lhs = Expr.App(Expr.Var(op), List(lhs, rhs))
    lhs

  def parseUnop =
    if lx.peek.isInstanceOf[Tok.Op] then
      val op = lx.next.asInstanceOf[Tok.Op]
      val rhs = parsePrimaryExpr
      Expr.App(Expr.Var(op), List(rhs))
    else parsePrimaryExpr

  def parseArgList(terminator: String = ")") =
    val args = mutable.ListBuffer.empty[Expr]
    while lx.peek != Tok.Key(terminator) do
      args += parseSingleExpr
      if lx.peek == Tok.Key(",") then lx.next
      else expectEq(lx.peek, Tok.Key(terminator))
    args.toList

  def parsePrimaryExpr =
    var expr = lx.peek match
      case Tok.Key("[") =>
        val l = lx.next
        val exprs = parseArgList("]")
        val r = lx.next
        expectEq(r, Tok.Key("]"))
        Expr.ListLit(l.asInstanceOf, exprs, r.asInstanceOf)

      case Tok.Key("(") =>
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

    while lx.peek == Tok.Key("(") || lx.peek == Tok.Key(".") do
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
