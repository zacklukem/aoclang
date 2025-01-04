package aoclang

trait TreeMod:
  def letl(value: LitValue)(body: Symbol => Tree): Tree =
    val sym = Symbol.local
    Tree.LetL(sym, value, body(sym))

  def letlNone(body: Symbol => Tree): Tree =
    letl(Sym.none)(body)

  def letp(prim: PrimOp, args: List[Symbol])(body: Symbol => Tree): Tree =
    val sym = Symbol.local
    Tree.LetP(sym, prim, args, body(sym))

  def letc(args: List[Symbol], value: Tree)(body: Symbol => Tree): Tree =
    val sym = Symbol.local
    Tree.LetC(sym, args, value, body(sym))

  def app(fn: Symbol, args: List[Symbol])(thenC: Symbol => Tree): Tree =
    val ret = Symbol.local
    letc(List(ret), thenC(ret)) { retC =>
      Tree.AppF(fn, retC, args)
    }

  def iff(cond: Symbol)(thenT: => Tree)(elseT: => Tree): Tree =
    letc(List(), thenT) { thenC =>
      letc(List(), elseT) { elseC =>
        Tree.If(cond, thenC, elseC)
      }
    }

  enum Tree:
    case AppF(fn: Symbol, retC: Symbol, args: List[Symbol])
    case AppC(fn: Symbol, args: List[Symbol])
    case LetC(name: Symbol, args: List[Symbol], value: Tree, body: Tree)
    case LetF(name: Symbol, args: List[Symbol], value: Tree, body: Tree)
    case LetL(name: Symbol, value: LitValue, body: Tree)
    case LetP(name: Symbol, prim: PrimOp, args: List[Symbol], body: Tree)
    case If(cond: Symbol, thenC: Symbol, elseC: Symbol)
    case Raise(value: Symbol)

    def size(): Int =
      this match
        case Tree.AppF(_, _, args)        => 1
        case Tree.AppC(_, args)           => 1
        case Tree.LetC(_, _, value, body) => 1 + value.size() + body.size()
        case Tree.LetF(_, _, value, body) => 1 + value.size() + body.size()
        case Tree.LetL(_, _, body)        => 1 + body.size()
        case Tree.LetP(_, _, args, body)  => 1 + body.size()
        case Tree.If(cond, thenC, elseC)  => 1
        case Tree.Raise(value)            => 1

    def resym(): Tree = this match
      case Tree.LetC(name, args, value, body) =>
        val newName = Symbol.local
        val newArgs = args.map { _ => Symbol.local }
        Tree.LetC(
          newName,
          newArgs,
          value.subst(args.zip(newArgs).toMap).resym(),
          body.subst(Map(name -> newName))
        )
      case Tree.LetL(name, value, body) =>
        val newName = Symbol.local
        Tree.LetL(newName, value, body.subst(Map(name -> newName)))
      case Tree.LetP(name, prim, args, body) =>
        val newName = Symbol.local
        Tree.LetP(newName, prim, args, body.subst(Map(name -> newName)))
      case Tree.AppF(fn, retC, args) =>
        Tree.AppF(fn, retC, args)
      case Tree.AppC(fn, args) =>
        Tree.AppC(fn, args)
      case Tree.If(cond, thenC, elseC) =>
        Tree.If(cond, thenC, elseC)
      case Tree.Raise(value) =>
        Tree.Raise(value)

      case Tree.LetF(name, args, value, body) => ???

    def subst(subst: Map[Symbol, Symbol]): Tree =
      def sub(s: Symbol): Symbol = subst.getOrElse(s, s)

      this match
        case Tree.AppF(fn, retC, args) => Tree.AppF(sub(fn), sub(retC), args.map(sub))
        case Tree.AppC(fn, args)       => Tree.AppC(sub(fn), args.map(sub))
        case Tree.LetC(name, args, value, body) =>
          Tree.LetC(sub(name), args.map(sub), value.subst(subst), body.subst(subst))
        case Tree.LetF(name, args, value, body) =>
          Tree.LetF(sub(name), args.map(sub), value.subst(subst), body.subst(subst))
        case Tree.LetL(name, value, body) => Tree.LetL(sub(name), value, body.subst(subst))
        case Tree.LetP(name, prim, args, body) =>
          Tree.LetP(sub(name), prim, args.map(sub), body.subst(subst))
        case Tree.If(cond, thenC, elseC) => Tree.If(sub(cond), sub(thenC), sub(elseC))
        case Tree.Raise(value)           => Tree.Raise(sub(value))

    def pretty(depth: Int = 0): Unit =
      def line(s: String) = println("  " * depth + s)

      this match
        case Tree.AppF(fn, retC, args) =>
          line(s"appf ${fn}(${args.mkString(",")}) -> ${retC}")
        case Tree.AppC(fn, args) =>
          line(s"appc ${fn}(${args.mkString(",")})")
        case Tree.LetC(name, args, value, body) =>
          line(s"letc ${name}(${args.mkString(",")}) = {")
          value.pretty(depth + 1)
          line("}")
          body.pretty(depth)
        case LetF(name, args, value, body) =>
          line(s"letf ${name}(${args.mkString(",")}) = {")
          value.pretty(depth + 1)
          line("}")
          body.pretty(depth)
        case Tree.LetL(name, value, body) =>
          line(s"letl ${name} = ${value}")
          body.pretty(depth)
        case Tree.LetP(name, prim, args, body) =>
          line(s"letp ${name} = ${prim}(${args.mkString(",")})")
          body.pretty(depth)
        case Tree.If(cond, thenC, elseC) =>
          line(s"if ${cond} then ${thenC} else ${elseC}")
        case Tree.Raise(value) =>
          line(s"raise ${value}")

  enum Decl:
    case Def(args: List[Symbol], body: Tree)

    def pretty(name: Symbol): Unit =
      this match
        case Decl.Def(args, body) =>
          println(s"def $name(${args.mkString(",")}) = {")
          body.pretty(1)
          println("}")

object High extends TreeMod
