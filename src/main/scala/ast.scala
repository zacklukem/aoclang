package aoclang

class Node(val span: Span)

type LitValue = String | Long | Double | Boolean | Sym

case class TopLevel(decls: List[Decl])

case class Intrinsic(override val span: Span) extends Node(span)

enum Decl(span: Span) extends Node(span):
  case Def(
      name: Tok.Id | Tok.Op,
      args: List[Pat],
      body: Expr | Intrinsic
  ) extends Decl(name.span.get ++ body.span)

enum Pat(span: Span) extends Node(span):
  case Bind(name: Tok.Id) extends Pat(name.span.get)

enum Expr(span: Span) extends Node(span):
  case App(fn: Expr, args: List[Expr])
      extends Expr(args.lastOption.map(_.span ++ fn.span).getOrElse(fn.span))

  case Let(binding: Pat, rhs: Expr, body: Option[Expr])
      extends Expr(binding.span ++ body.getOrElse(rhs).span)

  case Var(name: Tok.Id | Tok.Op) extends Expr(name.span.get)

  case Lit(value: Tok.Lit) extends Expr(value.span.get)

  case Block(l: Tok.Key, expr: Option[Expr], r: Tok.Key) extends Expr(l.span.get ++ r.span.get)

  case Discard(lhs: Expr, rhs: Expr) extends Expr(lhs.span ++ rhs.span)

  case If(cond: Expr, thenBranch: Expr, elseBranch: Option[Expr])
      extends Expr(cond.span ++ elseBranch.getOrElse(thenBranch).span)

  case Field(expr: Expr, field: Tok.Id) extends Expr(expr.span ++ field.span.get)
