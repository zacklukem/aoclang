package aoclang

import java.util.UUID
import scala.collection.mutable
import High.{Decl, Tree, letl, letp}

def hoist(decls: Map[Symbol, Decl]): Map[Symbol, Decl] =
  val hoister = Hoist()

  decls.map { (name, decl) =>
    decl match
      case Decl.Def(args, body) =>
        name -> Decl.Def(args, hoister.hoist(body)(using Map.empty))
  } ++ hoister.newDecls

private class Hoist:
  val newDecls: mutable.ListBuffer[(Symbol, High.Decl)] = mutable.ListBuffer[(Symbol, Decl)]()

  private def inSymbols(t: Tree): Set[Symbol] =
    val symbols = mutable.Set[Symbol]()
    def inSymbols(t: Tree): Unit =
      t match
        case Tree.AppF(fn, retC, args) =>
          symbols += retC
          symbols += fn
          symbols ++= args
        case Tree.AppC(fn, args) =>
          symbols += fn
          symbols ++= args
        case Tree.LetC(name, args, value, body) =>
          inSymbols(value)
          inSymbols(body)
        case Tree.LetF(name, args, value, body) =>
          inSymbols(value)
          inSymbols(body)
        case Tree.LetL(name, value, body) =>
          inSymbols(body)
        case Tree.LetP(name, prim, args, body) =>
          symbols ++= args
          inSymbols(body)
        case Tree.If(cond, thenC, elseC) =>
          symbols += cond
          symbols += thenC
          symbols += elseC
        case Tree.Raise(value) =>
          symbols += value

    inSymbols(t)
    symbols.toSet

  private def outSymbols(t: Tree): Set[Symbol] =
    val symbols = mutable.Set[Symbol]()
    def outSymbols(t: Tree): Unit =
      t match
        case Tree.LetC(name, args, value, body) =>
          symbols += name
          symbols ++= args
          outSymbols(value)
          outSymbols(body)
        case Tree.LetF(name, args, value, body) =>
          symbols += name
          symbols ++= args
          outSymbols(value)
          outSymbols(body)
        case Tree.LetL(name, value, body) =>
          symbols += name
          outSymbols(body)
        case Tree.LetP(name, prim, args, body) =>
          symbols += name
          outSymbols(body)
        case _ => ()

    outSymbols(t)
    symbols.toSet

  def hoist(t: Tree)(using subst: Map[Symbol, Symbol]): Tree =
    def sub(s: Symbol): Symbol =
      subst.getOrElse(s, s)

    t match
      case Tree.LetF(name, args, oldvalue, body) =>
        val value = hoist(oldvalue)
        val enclosed = (inSymbols(value) -- outSymbols(value) -- args).toList
          .filter(_.isInstanceOf[Symbol.Local])

        def loadEnclosed(enclosed: List[Int], env: Symbol)(
            c: List[Symbol] => Tree
        ): Tree =
          enclosed match
            case Nil =>
              c(Nil)
            case idx :: rest =>
              letl(idx.toLong) { idx =>
                letp(PrimOp.TupleGet, List(env, idx)) { x =>
                  loadEnclosed(rest, env) { closed =>
                    c(x :: closed)
                  }
                }
              }

        val defName = Symbol.Global(List("TODO", UUID.randomUUID().toString))
        if enclosed.isEmpty then
          newDecls.append(
            defName -> Decl.Def(args, value)
          )
        else
          val env = Symbol.local
          val envValues = enclosed.map { _ => Symbol.local }
          newDecls.append(
            defName -> Decl.Def(
              env :: args,
              loadEnclosed(enclosed.indices.toList, env) { closed =>
                value.subst(enclosed.zip(closed).toMap)
              }
            )
          )

        letp(PrimOp.ClosureNew, defName :: enclosed) { closure =>
          hoist(body)(using subst + (name -> closure))
        }

      case Tree.LetC(name, args, value, body) =>
        Tree.LetC(name, args, hoist(value), hoist(body))

      case Tree.LetL(name, value, body) =>
        Tree.LetL(name, value, hoist(body))

      case Tree.LetP(name, op, args, body) =>
        Tree.LetP(name, op, args.map(sub), hoist(body))

      case Tree.AppF(fn, retC, args) =>
        Tree.AppF(sub(fn), retC, args.map(sub))

      case Tree.AppC(c, args) =>
        Tree.AppC(c, args.map(sub))

      case Tree.If(cond, thenC, elseC) =>
        Tree.If(cond, thenC, elseC)

      case Tree.Raise(v) =>
        Tree.Raise(sub(v))
