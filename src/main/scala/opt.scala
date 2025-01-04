package aoclang

import scala.collection.mutable

case class State(
    useCount: Map[Symbol, Int],
    lit: Map[LitValue, Symbol] = Map.empty,
    subst: Map[Symbol, Symbol] = Map.empty,
    cnts: Map[Symbol, Tree.LetC] = Map.empty
):
  def count(s: Symbol) = useCount.getOrElse(s, 0)

  def sub(s: Symbol): Symbol =
    subst.getOrElse(s, s)

  def withSub(from: Symbol, to: Symbol): State =
    copy(subst = subst + (from -> to))

  def withSub(subs: Seq[(Symbol, Symbol)]): State =
    copy(subst = subst ++ subs)

  def withLit(litValue: LitValue, symbol: Symbol): State =
    copy(lit = lit + (litValue -> symbol))

  def withCnt(name: Symbol, cnt: Tree.LetC): State =
    copy(cnts = cnts + (name -> cnt))

def optimize(decls: Map[Symbol, LowDecl]): Map[Symbol, LowDecl] =
  val opt = Optimizer()

  decls
    .map({
      case (name, LowDecl.Def(args, body)) =>
        name -> LowDecl.Def(args, opt.opt(body))
      case decl => decl
    })

class Optimizer:
  def opt(t: Tree): Tree =
    val optT = shrinking(t)(using State(census(t)))
    if t == optT then t else opt(optT)

  def census(t: Tree): Map[Symbol, Int] =
    def census(t: Tree)(using useCount: mutable.Map[Symbol, Int]): Unit =
      def use(s: Symbol) = useCount.update(s, useCount.getOrElse(s, 0) + 1)

      t match
        case Tree.AppF(fn, retC, args) =>
          use(fn)
          use(retC)
          args.foreach(use)
        case Tree.AppC(fn, args) =>
          use(fn)
          args.foreach(use)
        case Tree.If(cond, thenC, elseC) =>
          use(cond)
          use(thenC)
          use(elseC)
        case Tree.Raise(value) =>
          use(value)
        case Tree.LetC(name, args, value, body) =>
          census(value)
          census(body)
        case Tree.LetL(name, value, body) =>
          census(body)
        case Tree.LetP(name, prim, args, body) =>
          args.foreach(use)
          census(body)

    val res = mutable.Map.empty[Symbol, Int]
    census(t)(using res)
    res.toMap

  private def shrinking(t: Tree)(using state: State): Tree =
    t match
      // Literal propagation
      case Tree.LetL(name, value, body) if state.lit.contains(value) =>
        shrinking(body)(using state.withSub(name, state.lit(value)))

      // Dead code elimination
      case Tree.LetL(name, value, body) if state.count(name) == 0 =>
        shrinking(body)
      case Tree.LetC(name, args, value, body) if state.count(name) == 0 =>
        shrinking(body)
      case Tree.LetP(name, op, args, body) if state.count(name) == 0 && pure(op) =>
        shrinking(body)

      // Shrinking inlining
      case Tree.AppC(fn, args) if fn != Symbol.Ret && state.count(fn) == 1 =>
        val Tree.LetC(_, params, body, _) = state.cnts(fn)
        shrinking(body)(using state.withSub(params.zip(args)))

      case Tree.LetL(name, value, body) =>
        Tree.LetL(name, value, shrinking(body)(using state.withLit(value, name)))
      case cnt @ Tree.LetC(name, args, value, body) =>
        Tree.LetC(name, args, shrinking(value), shrinking(body)(using state.withCnt(name, cnt)))
      case Tree.LetP(name, op, args, body) =>
        Tree.LetP(name, op, args.map(state.sub), shrinking(body))
      case Tree.AppF(fn, retC, args) =>
        Tree.AppF(state.sub(fn), state.sub(retC), args.map(state.sub))
      case Tree.AppC(fn, args) =>
        Tree.AppC(state.sub(fn), args.map(state.sub))
      case Tree.If(cond, thenC, elseC) =>
        Tree.If(state.sub(cond), state.sub(thenC), state.sub(elseC))
      case Tree.Raise(value) =>
        Tree.Raise(state.sub(value))

      case Tree.LetF(name, args, value, body) => throw new Error()

def pure(prim: PrimOp) = prim match
  case PrimOp.PrintLine | PrimOp.Assert => false
  case _                                => true
