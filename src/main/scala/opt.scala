package aoclang

case class State(
    lit: Map[LitValue, Symbol] = Map.empty,
    subst: Map[Symbol, Symbol] = Map.empty
):
  def sub(s: Symbol): Symbol =
    subst.getOrElse(s, s)

  def withSub(from: Symbol, to: Symbol): State =
    copy(subst = subst + (from -> to))

  def withLit(litValue: LitValue, symbol: Symbol): State =
    copy(lit = lit + (litValue -> symbol))

class Optimizer:
  def opt(t: Tree): Tree =
    val optT = shrinking(t)(using State())
    if t == optT then t else opt(optT)

  private def shrinking(t: Tree)(using state: State): Tree =
    t match
      case Tree.LetL(name, value, body) if state.lit.contains(value) =>
        shrinking(body)(using state.withSub(name, state.lit(value)))
      case Tree.LetL(name, value, body) =>
        Tree.LetL(name, value, shrinking(body)(using state.withLit(value, name)))
      case Tree.LetC(name, args, value, body) =>
        Tree.LetC(name, args, shrinking(value), shrinking(body))
      case Tree.LetF(name, args, value, body) =>
        // TODO: Hoisting should happen before (if ever)
        Tree.LetF(name, args, shrinking(value), shrinking(body))
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
