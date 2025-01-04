package aoclang

import scala.annotation.tailrec
import Low.*;

enum Value:
  case Tuple(vs: Array[Value])
  case ListVal(vs: List[Value])
  case Lit(v: LitValue)
  case Closure(fn: Symbol.Global, env: Option[Value])
  case Cnt(args: List[Name], body: Tree)

class Interp(val decls: Map[Symbol, Decl]):
  private def evalNonTail(e: Tree)(using frame: Array[Value], stack: List[Name]): Value =
    eval(e)

  @tailrec
  final def eval(e: Tree)(using frame: Array[Value], stack: List[Name]): Value =
    def appc_env(fn: Int, args: List[Value]) =
      val Value.Cnt(argSyms, body) = frame(fn)
      argSyms.zip(args).foreach { case (k: Int, v) =>
        frame(k) = v
      }
      body

    def genv(s: Name): Value =
      s match
        case s: Int           => frame(s)
        case s: Symbol.Global => Value.Closure(s, None)

    def setupAppCall(fn: Name, args: List[Name]) =
      fn match
        case fn: Symbol.Global =>
          val decl @ Decl.Def(argSyms, tree) = decls(fn)
          val fenv = Array.ofDim[Value](decl.maxStack.get)
          argSyms.zip(args.map(genv)).foreach { case (k: Int, v) => fenv(k) = v }
          (tree, fenv, fn :: stack)

        case fn: Int =>
          frame(fn) match
            case Value.Closure(fn, Some(cenv)) =>
              val decl @ Decl.Def(argSyms, tree) = decls(fn)
              val fenv = Array.ofDim[Value](decl.maxStack.get)
              argSyms.zip(cenv :: args.map(genv)).foreach { case (k: Int, v) => fenv(k) = v }
              (tree, fenv, fn :: stack)
            case Value.Closure(fn, None) =>
              val decl @ Decl.Def(argSyms, tree) = decls(fn)
              val fenv = Array.ofDim[Value](decl.maxStack.get)
              argSyms.zip(args.map(genv)).foreach { case (k: Int, v) => fenv(k) = v }
              (tree, fenv, fn :: stack)

    e match
      case Tree.AppF(fn, Symbol.Ret, args) =>
        val (tree, frame, stack) = setupAppCall(fn, args)
        eval(tree)(using frame, stack)

      case Tree.AppF(fn, retC, args) =>
        val (tree, frame, stack) = setupAppCall(fn, args)
        val res = evalNonTail(tree)(using frame, stack)
        val body = appc_env(retC.asInstanceOf, List(res))
        eval(body)

      case Tree.LetP(name: Int, PrimOp.ClosureNew, (fn: Symbol.Global) :: cenv, body) =>
        val cenvVal = if cenv == Nil then None else Some(Value.Tuple(cenv.map(genv).toArray))
        val v = Value.Closure(fn, cenvVal)
        frame(name) = v
        eval(body)

      case Tree.LetP(name: Int, op, args, body) =>
        val v =
          try INTRINSICS(op)(args.map(genv))
          catch case Xcept(msg) => throw XceptWithStack(msg, stack)
        frame(name) = v
        eval(body)

      case Tree.AppC(Symbol.Ret, rawArgs) =>
        val args = rawArgs.map(genv)
        args.head

      case Tree.AppC(fn: Int, rawArgs) =>
        val args = rawArgs.map(genv)
        val body = appc_env(fn, args)
        eval(body)

      case Tree.LetC(name: Int, args, value, body) =>
        val cnt = Value.Cnt(args, value)
        frame(name) = cnt
        eval(body)

      case Tree.LetL(name: Int, value, body) =>
        val v = Value.Lit(value)
        frame(name) = v
        eval(body)

      case Tree.If(cond, thenC: Int, elseC: Int) =>
        val Value.Lit(b: Boolean) = genv(cond)
        if b then
          val body = appc_env(thenC, Nil)
          eval(body)
        else
          val body = appc_env(elseC, Nil)
          eval(body)

      case Tree.Raise(v) =>
        try xcept(s"raised: ${genv(v)}")
        catch case Xcept(msg) => throw XceptWithStack(msg, stack)

      case Tree.LetF(name, args, value, body) => throw Error()
