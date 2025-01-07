package aoclang

import scala.collection.mutable
import High.*

import scala.annotation.tailrec

def pure(prim: PrimOp) = prim match
  case PrimOp.PrintLine | PrimOp.Assert | PrimOp.AssertEq | PrimOp.Store => false
  case _                                                                 => true

case class State(
    useCount: Map[Symbol, Int],
    lit: Map[LitValue, Symbol] = Map.empty,
    prim: Map[(PrimOp, List[Symbol]), Symbol] = Map.empty,
    subst: Map[Symbol, Symbol] = Map.empty,
    cnts: Map[Symbol, Tree.LetC] = Map.empty
):
  def count(s: Symbol): Int = useCount.getOrElse(s, 0)

  def sub(s: Symbol): Symbol =
    subst.getOrElse(s, s)

  def withSub(from: Symbol, to: Symbol): State =
    copy(subst = subst + (from -> to))

  def withSub(subs: Seq[(Symbol, Symbol)]): State =
    copy(subst = subst ++ subs)

  def withLit(litValue: LitValue, symbol: Symbol): State =
    copy(lit = lit + (litValue -> symbol))

  def withPrim(op: PrimOp, args: List[Symbol], symbol: Symbol): State =
    copy(prim = prim + ((op, args) -> symbol))

  def withCnt(name: Symbol, cnt: Tree.LetC): State =
    copy(cnts = cnts + (name -> cnt))

def optimize(decls: Map[Symbol, Decl]): Map[Symbol, Decl] =
  val opt = Optimizer(mutable.Map.from(decls))
  opt.opt()
  opt.decls.toMap

class Optimizer(val decls: mutable.Map[Symbol, Decl]):
  def opt(): Unit =
    decls.foreach {
      case (name, Decl.Def(args, body)) =>
        decls(name) = Decl.Def(
          args,
          body
            |> shrink
            |> { t => inlining(t)(using 1) }
            |> shrink
            |> { t => inlining(t)(using 2) }
            |> shrink
        )
      case _ =>
    }

  private def inlining(t: Tree)(using limit: Int): Tree =
    t match
      case Tree.AppF(fn: Symbol.Global, retC, args) =>
        val Decl.Def(params, body) = decls(fn)
        if body.size() <= limit then
          val subs = ((Symbol.Ret -> retC) :: params.zip(args)).toMap
          body.resym(Symbol.local).subst(subs)
        else Tree.AppF(fn, retC, args)

      case Tree.LetC(name, args, value, body) =>
        Tree.LetC(name, args, inlining(value), inlining(body))
      case Tree.LetL(name, value, body) =>
        Tree.LetL(name, value, inlining(body))
      case Tree.LetP(name, prim, args, body) =>
        Tree.LetP(name, prim, args, inlining(body))
      case t => t

  @tailrec
  private def shrink(t: Tree): Tree =
    val optT = shrinking(t)(using State(census(t)))
    if t == optT then t else shrink(optT)

  private def shrinking(t: Tree)(using state: State): Tree =
    t match
      // CSE
      case Tree.LetL(name, value, body) if state.lit.contains(value) =>
        shrinking(body)(using state.withSub(name, state.lit(value)))
      case Tree.LetP(name, op, args, body) if pure(op) && state.prim.contains((op, args)) =>
        shrinking(body)(using state.withSub(name, state.prim((op, args))))

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
        body.subst(params.zip(args).toMap)

      case Tree.LetL(name, value, body) =>
        Tree.LetL(name, value, shrinking(body)(using state.withLit(value, name)))
      case cnt @ Tree.LetC(name, args, value, body) =>
        Tree.LetC(name, args, shrinking(value), shrinking(body)(using state.withCnt(name, cnt)))
      case Tree.LetP(name, op, args, body) =>
        Tree.LetP(
          name,
          op,
          args.map(state.sub),
          shrinking(body)(using state.withPrim(op, args, name))
        )
      case Tree.AppF(fn, retC, args) =>
        Tree.AppF(state.sub(fn), state.sub(retC), args.map(state.sub))
      case Tree.AppC(fn, args) =>
        Tree.AppC(state.sub(fn), args.map(state.sub))
      case Tree.If(cond, thenC, elseC) =>
        Tree.If(state.sub(cond), state.sub(thenC), state.sub(elseC))
      case Tree.Raise(value) =>
        Tree.Raise(state.sub(value))

      case Tree.LetF(name, args, value, body) => throw new Error()

  private def census(t: Tree): Map[Symbol, Int] =
    def census(t: Tree)(using useCount: mutable.Map[Symbol, Int]): Unit =
      def use(s: Symbol): Unit = useCount.updateWith(s) {
        case None    => Some(1)
        case Some(c) => Some(c + 1)
      }

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
