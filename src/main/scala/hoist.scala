package aoclang

import java.util.UUID
import scala.collection.mutable
import High.{Tree, letl, letp, Decl}

def hoist(decls: Map[Symbol, Decl]): Map[Symbol, Decl] =
  val hoister = Hoist()

  decls.map { (name, decl) =>
    decl match
      case Decl.Def(args, body) =>
        name -> Decl.Def(args, hoister.hoist(body)(using Map.empty))
  } ++ hoister.newDecls

private class Hoist:
  val newDecls = mutable.ListBuffer[(Symbol, Decl)]()

  def inSymbols(t: Tree): Set[Symbol] =
    t match
      case Tree.AppF(fn, retC, args) =>
        Set.from(retC :: fn :: args)
      case Tree.AppC(fn, args) =>
        Set.from(fn :: args)
      case Tree.LetC(name, args, value, body) =>
        inSymbols(body) ++ inSymbols(value)
      case Tree.LetF(name, args, value, body) =>
        inSymbols(body) ++ inSymbols(value)
      case Tree.LetL(name, value, body) =>
        inSymbols(body)
      case Tree.LetP(name, prim, args, body) =>
        inSymbols(body) ++ args
      case Tree.If(cond, thenC, elseC) =>
        Set(cond, thenC, elseC)
      case Tree.Raise(value) =>
        Set(value)

  def outSymbols(t: Tree): Set[Symbol] =
    t match
      case Tree.AppF(fn, retC, args) =>
        Set.empty
      case Tree.AppC(fn, args) =>
        Set.empty
      case Tree.LetC(name, args, value, body) =>
        outSymbols(value) ++ outSymbols(body) ++ args + name
      case Tree.LetF(name, args, value, body) =>
        outSymbols(value) ++ outSymbols(body) ++ args + name
      case Tree.LetL(name, value, body) =>
        outSymbols(body) + name
      case Tree.LetP(name, prim, args, body) =>
        outSymbols(body) + name
      case Tree.If(cond, thenC, elseC) =>
        Set.empty
      case Tree.Raise(value) =>
        Set.empty

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
