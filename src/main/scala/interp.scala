package aoclang

import scala.annotation.tailrec

enum Value:
  case Tuple(vs: Array[Value])
  case Lit(v: LitValue)
  case FnRef(v: Symbol.Global)
  case Cnt(args: List[Symbol], body: Tree, env: Map[Symbol, Value])

class Interp(val decls: Map[Symbol, LowDecl]):
  private def evalNonTail(e: Tree)(using env: Map[Symbol, Value]): Value = eval(e)

  @tailrec
  final def eval(e: Tree)(using env: Map[Symbol, Value]): Value =
    def deepDecl(s: Symbol): LowDecl = decls.get(s) match
      case Some(d) => d
      case None =>
        val Value.FnRef(ref) = env(s)
        deepDecl(ref)

    def appc_env(fn: Symbol, args: List[Value]) =
      val Value.Cnt(argSyms, body, fenv) = env(fn)
      val fenvp = fenv ++ argSyms.zip(args).toMap
      (body, fenvp)

    def genv(s: Symbol): Value =
      env.getOrElse(
        s, {
          val gs = s.asInstanceOf[Symbol.Global]
          Value.FnRef(gs)
        }
      )

    e match
      case Tree.AppF(fn, Symbol.Ret, args) =>
        deepDecl(fn) match
          case LowDecl.Def(argSyms, tree) =>
            val fenv = argSyms.zip(args.map(genv)).toMap
            eval(tree)(using fenv)
          case LowDecl.Intrinsic(f) =>
            INTRINSICS(f)(args.map(genv))

      case Tree.AppF(fn, retC, args) =>
        val res = deepDecl(fn) match
          case LowDecl.Def(argSyms, tree) =>
            val fenv = argSyms.zip(args.map(genv)).toMap
            evalNonTail(tree)(using fenv)
          case LowDecl.Intrinsic(f) =>
            INTRINSICS(f)(args.map(genv))

        if retC == Symbol.Ret then res
        else
          val (body, fenvp) = appc_env(retC, List(res))
          eval(body)(using fenvp)

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
        throw new Exception(s"raised: ${genv(v)}")
