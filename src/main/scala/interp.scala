package aoclang

enum Value:
  case Lit(v: LitValue)
  case FnRef(v: Symbol.Global)
  case Cnt(args: List[Symbol], body: Tree, env: Map[Symbol, Value])

class Interp(val decls: Map[Symbol, LowDecl]):

  def eval(e: Tree)(using env: Map[Symbol, Value]): Value =
    def deepDecl(s: Symbol): LowDecl = decls.get(s) match
      case Some(d) => d
      case None =>
        val Value.FnRef(ref) = env(s)
        deepDecl(ref)

    def appc(fn: Symbol, args: List[Value]): Value =
      val Value.Cnt(argSyms, body, fenv) = env(fn)
      val fenvp = fenv ++ argSyms.zip(args).toMap

      eval(body)(using fenvp)

    def genv(s: Symbol): Value =
      env.getOrElse(
        s, {
          val gs = s.asInstanceOf[Symbol.Global]
          Value.FnRef(gs)
        }
      )

    e match
      case Tree.AppF(fn, retC, args) =>
        val res = deepDecl(fn) match
          case LowDecl.Def(argSyms, tree) =>
            val fenv = argSyms.zip(args.map(genv)).toMap
            eval(tree)(using fenv)
          case LowDecl.Intrinsic(f) =>
            INTRINSICS(f)(args.map(genv))

        appc(retC, List(res))

      case Tree.AppC(fn, args) =>
        appc(fn, args.map(genv))

      case Tree.LetC(name, args, value, body) =>
        val cnt = Value.Cnt(args, value, env)
        eval(body)(using env + (name -> cnt))

      case Tree.LetL(name, value, body) =>
        val v = Value.Lit(value)
        eval(body)(using env + (name -> v))

      case Tree.If(cond, thenC, elseC) =>
        val Value.Lit(b: Boolean) = genv(cond)
        if b then appc(thenC, Nil)
        else appc(elseC, Nil)

      case Tree.Ret(value) =>
        genv(value)
