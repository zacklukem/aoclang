package aoclang

import scala.annotation.tailrec
import High.*;

enum Value:
  case Tuple(vs: Array[Value])
  case ListVal(vs: List[Value])
  case Lit(v: LitValue)
  case Closure(fn: Symbol.Global, env: Option[Value])
  case Cnt(args: List[Symbol], body: Tree, env: Map[Symbol, Value])

class Interp(val decls: Map[Symbol, Decl]):
  private def evalNonTail(e: Tree)(using env: Map[Symbol, Value], stack: List[Symbol]): Value =
    eval(e)

  @tailrec
  final def eval(e: Tree)(using env: Map[Symbol, Value], stack: List[Symbol]): Value =
    def appc_env(fn: Symbol, args: List[Value]) =
      val Value.Cnt(argSyms, body, fenv) = env(fn)
      val fenvp = fenv ++ argSyms.zip(args).toMap
      (body, fenvp)

    def genv(s: Symbol): Value =
      s match
        case s: Symbol.Local  => env(s)
        case s: Symbol.Global => Value.Closure(s, None)

    e match
      case Tree.AppF(fn, Symbol.Ret, args) =>
        fn match
          case fn: Symbol.Global =>
            val Decl.Def(argSyms, tree) = decls(fn)
            val fenv = argSyms.zip(args.map(genv)).toMap
            eval(tree)(using fenv, fn :: stack)

          case fn =>
            env(fn) match
              case Value.Closure(fn, Some(cenv)) =>
                val Decl.Def(argSyms, tree) = decls(fn)
                val fenv = argSyms.zip(cenv :: args.map(genv)).toMap
                eval(tree)(using fenv, fn :: stack)
              case Value.Closure(fn, None) =>
                val Decl.Def(argSyms, tree) = decls(fn)
                val fenv = argSyms.zip(args.map(genv)).toMap
                eval(tree)(using fenv, fn :: stack)

      case Tree.AppF(fn, retC, args) =>
        val res = fn match
          case fn: Symbol.Global =>
            val Decl.Def(argSyms, tree) = decls(fn)
            val fenv = argSyms.zip(args.map(genv)).toMap
            evalNonTail(tree)(using fenv, fn :: stack)

          case fn =>
            env(fn) match
              case Value.Closure(fn, Some(cenv)) =>
                val Decl.Def(argSyms, tree) = decls(fn)
                val fenv = argSyms.zip(cenv :: args.map(genv)).toMap
                evalNonTail(tree)(using fenv, fn :: stack)
              case Value.Closure(fn, None) =>
                val Decl.Def(argSyms, tree) = decls(fn)
                val fenv = argSyms.zip(args.map(genv)).toMap
                evalNonTail(tree)(using fenv, fn :: stack)

        val (body, fenvp) = appc_env(retC, List(res))
        eval(body)(using fenvp)

      case Tree.LetP(name, PrimOp.ClosureNew, (fn: Symbol.Global) :: cenv, body) =>
        val cenvVal = if cenv == Nil then None else Some(Value.Tuple(cenv.map(genv).toArray))
        val v = Value.Closure(fn, cenvVal)
        eval(body)(using env + (name -> v))

      case Tree.LetP(name, op, args, body) =>
        val v =
          try INTRINSICS(op)(args.map(genv))
          catch case Xcept(msg) => throw XceptWithStack(msg, stack)
        eval(body)(using env + (name -> v))

      case Tree.AppC(fn, rawArgs) =>
        val args = rawArgs.map(genv)
        if fn == Symbol.Ret then args.head
        else
          val (body, fenvp) = appc_env(fn, args)
          eval(body)(using fenvp)

      case Tree.LetC(name, args, value, body) =>
        val cnt = Value.Cnt(args, value, env)
        eval(body)(using env + (name -> cnt))

      case Tree.LetL(name, value, body) =>
        val v = Value.Lit(value)
        eval(body)(using env + (name -> v))

      case Tree.If(cond, thenC, elseC) =>
        val Value.Lit(b: Boolean) = genv(cond)
        if b then
          val (body, fenvp) = appc_env(thenC, Nil)
          eval(body)(using fenvp)
        else
          val (body, fenvp) = appc_env(elseC, Nil)
          eval(body)(using fenvp)

      case Tree.Raise(v) =>
        try xcept(s"raised: ${genv(v)}")
        catch case Xcept(msg) => throw XceptWithStack(msg, stack)

      case Tree.LetF(name, args, value, body) => throw Error()
