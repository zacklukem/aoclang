package aoclang

import aoclang.High as H
import aoclang.Low as L

def regalloc(decls: Map[Symbol, H.Decl]): Map[Symbol, L.Decl] =
  decls.map { (name, decl) =>
    name -> regalloc(decl)
  }

def regalloc(tree: H.Tree): L.Tree =
  var reg = 0

  def nextReg =
    reg += 1
    reg

  regalloc(tree, Map.empty, nextReg)

private def regalloc(tree: H.Tree, subst: Map[H.Name, L.Name], nextReg: => Int): L.Tree =
  def walk(t: H.Tree)(using subst: Map[H.Name, L.Name]): L.Tree =
    def sub4(name: H.Name): L.Name =
      name match
        case Symbol.Local(id)    => nextReg
        case Symbol.Global(name) => Symbol.Global(name)
        case Symbol.Ret          => Symbol.Ret

    def sub(name: H.Name): L.Name = subst.getOrElse(name, sub4(name))

    t match
      case H.Tree.AppF(fn, retC, args) =>
        L.Tree.AppF(sub(fn), sub(retC), args.map(sub))
      case H.Tree.AppC(fn, args) =>
        L.Tree.AppC(sub(fn), args.map(sub))
      case H.Tree.LetC(name, args, value, body) =>
        val newName = sub4(name)
        val newArgs = args.map(sub4)

        L.Tree.LetC(
          newName,
          newArgs,
          walk(value)(using subst ++ args.zip(newArgs)),
          walk(body)(using subst + (name -> newName))
        )

      case H.Tree.LetL(name, value, body) =>
        val newName = sub4(name)
        L.Tree.LetL(newName, value, walk(body)(using subst + (name -> newName)))

      case H.Tree.LetP(name, prim, args, body) =>
        val newName = sub4(name)
        L.Tree.LetP(newName, prim, args.map(sub), walk(body)(using subst + (name -> newName)))

      case H.Tree.If(cond, thenC, elseC) =>
        L.Tree.If(sub(cond), sub(thenC), sub(elseC))

      case H.Tree.Raise(value) =>
        L.Tree.Raise(sub(value))

      case H.Tree.LetF(name, args, value, body) => throw Error()

  walk(tree)(using subst)

private def regalloc(decl: H.Decl): L.Decl =
  val H.Decl.Def(params, body) = decl

  var reg = 0

  def nextReg =
    reg += 1
    reg

  val newParams = params.map { name => nextReg }
  val newBody = regalloc(body, params.zip(newParams).toMap, nextReg)
  L.Decl.Def(newParams, newBody).withMaxStack(reg + 1)
