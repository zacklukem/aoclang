package aoclang

import Low.*

import java.io.PrintWriter

val operatorClean = Map(
  '+' -> "OPplus",
  '-' -> "OPminus",
  '*' -> "OPstar",
  '/' -> "OPslash",
  '=' -> "OPequals",
  '!' -> "OPbang",
  '<' -> "OPless_than",
  '>' -> "OPgreater_than",
  '&' -> "OPampersand",
  '|' -> "OPbar",
  '^' -> "OPcaret",
  '~' -> "OPtilde",
  '%' -> "OPpercent",
  '?' -> "OPquestion",
  ':' -> "OPcolon",
  '.' -> "OPdot",
  ',' -> "OPcomma",
  '@' -> "OPat_sign"
)

def cleanGlobal(s: String) =
  s.map {
    case c if c.isLetterOrDigit => c
    case '_'                    => '_'
    case c                      => operatorClean(c)
  }.mkString

def cleanBuiltin(s: String) =
  s.map {
    case c if c.isUpper => s"_${c.toLower}"
    case c              => c
  }.mkString

class Gen(f: PrintWriter):
  def write(s: String): Unit =
    f.println(s)
    f.flush()

  def writeInst(s: String): Unit =
    f.println("\t" + s)
    f.flush()

  def genAll(decls: Map[Symbol, Decl]) =
    write(s"#include <aoclang/runtime.h>")

    decls.foreach { case (name, Decl.Def(args, tree)) =>
      write(
        s"value_t ${genName(name)}(runtime_t *rt${args.map(v => s", value_t _arg_$v").mkString});"
      )
      write(s"value_t _al_closure_${genName(name)} = {._0 = 0, ._1 = (uint64_t)${genName(name)}};")
    }
    decls.foreach { case (name, Decl.Def(args, tree)) =>
      write(
        s"value_t ${genName(name)}(runtime_t *rt${args.map(v => s", value_t _arg_$v").mkString}) {"
      )
      writeInst("value_t _ret;")
      writeInst(s"struct {")
      args.foreach { arg =>
        writeInst(s"\tvalue_t _$arg;")
      }
      // TODO: find max temp size
      val ntmp = maxTmpArr(tree, name)
      val nloc = genLocals(tree) + args.length + ntmp
      writeInst(s"\tvalue_t _tmp_arr[$ntmp];")
      writeInst("} locals;")
      writeInst(s"_al_enter_frame(rt, $nloc, (void*)&locals);")
      write("_tailcall:")
      args.foreach { arg =>
        writeInst(s"${genName(arg)} = _arg_$arg;")
      }
      genBody(tree)(using Nil, Map.empty, name, args)
      write("}")
    }

    write(s"int main(int argc, char **argv) {")
    decls.toSeq
      .filter((name, _) => isTest(name))
      .sortBy(_._1.toString)
      .foreach { case (name, Decl.Def(args, tree)) =>
        writeInst(s"_al_test_harness((void*)${genName(name)}, \"$name\");")
      }
    write("}")

  def genName(n: Name): String =
    n match
      case Symbol.Global(g) => s"__${g.map(cleanGlobal).mkString("_")}"
      case v: Int           => s"locals._$v"
      case _                => "!$!ERROR!$!"

  def genNameClosed(n: Name): String =
    n match
      case n: Symbol.Global => s"_al_closure_${genName(n)}"
      case n                => genName(n)

  def genLocals(t: Tree): Int =
    var i = 0;
    def use(n: Name) =
      i += 1;
      writeInst(s"\tvalue_t _$n;")

    def genLocals(t: Tree): Unit =
      t match
        case Tree.LetC(_name, args, value, body) =>
          args.foreach(use)
          genLocals(value)
          genLocals(body)
        case Tree.LetL(name, _value, body) =>
          use(name)
          genLocals(body)
        case Tree.LetP(name, _prim, _args, body) =>
          use(name)
          genLocals(body)
        case _ => ()
    genLocals(t)
    i

  // String | Long | Double | Boolean | Sym
  def genValue(v: LitValue): String =
    v match
      case s: String  => s"_al_string_new(rt, \"${s.replaceAll("\n", "\\n")}\")"
      case i: Long    => s"_al_int_new(rt, ${i}LL)"
      case d: Double  => s"_al_float_new(rt, ${d})"
      case b: Boolean => if b then "_al_true_new(rt)" else "_al_false_new(rt)"
      case Sym(v)     => s"_al_sym_new(rt, \"$v\")"

  def genTempArray(vs: Seq[Name]) =
    vs.zipWithIndex.foreach { case (v, i) =>
      writeInst(s"locals._tmp_arr[$i] = ${genNameClosed(v)};")
    }

  def maxTmpArr(t: Tree, tailName: Symbol): Int =
    var max = 0
    def maxTmpArr(t: Tree): Unit =
      t match
        case Tree.LetC(name, args, value, body) =>
          maxTmpArr(value)
          maxTmpArr(body)

        case Tree.LetL(name, value, body) =>
          maxTmpArr(body)

        case Tree.LetP(name, PrimOp.ClosureNew, fn :: args, body) =>
          max = max.max(args.length)
          maxTmpArr(body)

        case p @ Tree.LetP(name, PrimOp.ListNew | PrimOp.TupleNew, args, body) =>
          max = max.max(args.length)
          maxTmpArr(body)

        case Tree.LetP(name, prim, args, body) =>
          maxTmpArr(body)

        case Tree.AppF(fn, retC, args) if fn != tailName =>
          max = max.max(args.length)

        case _ => ()

    maxTmpArr(t)
    max

  def genBody(
      t: Tree
  )(using
      queue: List[(Name, Tree)],
      cEnv: Map[Name, List[Name]],
      tailName: Symbol,
      fnArgs: List[Name]
  ): Unit =
    def continue() =
      queue.foreach { case (cName, cTree) =>
        write(s"_$cName:")
        genBody(cTree)(using Nil)
      }

    t match
      case Tree.LetC(name, args, value, body) =>
        genBody(body)(using (name, value) :: queue, cEnv + (name -> args))

      case Tree.LetL(name, value, body) =>
        writeInst(s"${genName(name)} = ${genValue(value)};")
        genBody(body)

      case Tree.LetP(name, PrimOp.ClosureNew, fn :: Nil, body) =>
        writeInst(s"${genName(name)} = _al_closure_new_empty(rt, (void*)${genName(fn)});")
        genBody(body)

      case Tree.LetP(name, PrimOp.ClosureNew, fn :: args, body) =>
        genTempArray(args)
        writeInst(
          s"${genName(name)} = _al_closure_new(rt, (void*)${genName(fn)}, ${args.length}, locals._tmp_arr);"
        )
        genBody(body)

      case p @ Tree.LetP(name, PrimOp.ListNew | PrimOp.TupleNew, args, body) =>
        genTempArray(args)
        writeInst(
          s"${genName(name)} = _al${cleanBuiltin(p.prim.toString)}(rt, ${args.length}, locals._tmp_arr);"
        )
        genBody(body)

      case Tree.LetP(name, prim, args, body) =>
        writeInst(
          s"${genName(name)} = _al${cleanBuiltin(prim.toString)}(rt${args.map(v => s", ${genNameClosed(v)}").mkString});"
        )
        genBody(body)

      case Tree.AppF(name, Symbol.Ret, args) if name == tailName =>
        fnArgs.zip(args).foreach { case (fnArg, arg) =>
          writeInst(s"_arg_$fnArg = ${genNameClosed(arg)};")
        }
        writeInst("goto _tailcall;")
        continue()

      case Tree.AppF(name: Symbol.Global, Symbol.Ret, args) =>
        writeInst("_al_exit_frame(rt);")
        writeInst(
          s"return ${genName(name)}(rt${args.map(v => s", ${genNameClosed(v)}").mkString});"
        )
        continue()

      case Tree.AppF(name: Symbol.Global, retC, args) =>
        val List(retN) = cEnv(retC)
        writeInst(
          s"${genName(retN)} = ${genName(name)}(rt${args.map(v => s", ${genNameClosed(v)}").mkString});"
        )
        writeInst(s"goto _$retC;")
        continue()

      case Tree.AppF(fn, Symbol.Ret, args) =>
        genTempArray(args)
        writeInst("_al_exit_frame(rt);")
        writeInst(
          s"return _al_call(rt, ${genName(fn)}, ${args.length}, locals._tmp_arr);"
        )
        continue()

      case Tree.AppF(fn, retC, args) =>
        val List(retN) = cEnv(retC)
        genTempArray(args)
        writeInst(
          s"${genName(retN)} = _al_call(rt, ${genName(fn)}, ${args.length}, locals._tmp_arr);"
        )
        writeInst(s"goto _$retC;")
        continue()

      case Tree.AppC(Symbol.Ret, List(arg)) =>
        writeInst("_al_exit_frame(rt);")
        writeInst(s"return ${genName(arg)};")
        continue()

      case Tree.AppC(fn, args) =>
        cEnv(fn).zip(args).foreach { case (dest, src) =>
          writeInst(s"${genName(dest)} = ${genNameClosed(src)};")
        }
        writeInst(s"goto _$fn;")
        continue()

      case Tree.If(cond, thenC, elseC) =>
        writeInst(s"if (_al_get_bool(rt, ${genNameClosed(cond)})) goto _$thenC;")
        writeInst(s"else goto _$elseC;")
        continue()

      case Tree.Raise(value) =>
        writeInst(s"_al_raise(${genNameClosed(value)});")
        writeInst(s"return (value_t){0, 0};")
        continue()

      case Tree.LetF(name, args, value, body) => !!!
