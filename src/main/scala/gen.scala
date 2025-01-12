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
        s"value_t ${genName(name)}(runtime_t *rt${args.map(genName).map(v => s", value_t ${v}").mkString});"
      )
      write(s"value_t _al_closure_${genName(name)} = {._0 = 0, ._1 = (uint64_t)${genName(name)}};")
    }
    decls.foreach { case (name, Decl.Def(args, tree)) =>
      write(
        s"value_t ${genName(name)}(runtime_t *rt${args.map(genName).map(v => s", value_t ${v}").mkString}) {"
      )
      genLocals(tree)
      // TODO: find max temp size
      writeInst("value_t _tmp_arr[50];")
      genBody(tree)(using Nil, Map.empty)
      write("}")
    }

    write(s"int main(int argc, char **argv) {")
    writeInst("const runtime_t *rt = _al_runtime_new();")
    decls.toSeq
      .filter((name, _) => isTest(name))
      .sortBy(_._1.toString)
      .foreach { case (name, Decl.Def(args, tree)) =>
        writeInst(s"_al_test_harness(rt, (void*)${genName(name)}, \"$name\");")
      }
    write("}")

  def genName(n: Name): String =
    n match
      case Symbol.Global(g) => s"__${g.map(cleanGlobal).mkString("_")}"
      case v: Int           => s"_$v"
      case _                => "!$!ERROR!$!"

  def genNameClosed(n: Name): String =
    n match
      case n: Symbol.Global => s"_al_closure_${genName(n)}"
      case n                => genName(n)

  def genLocals(t: Tree): Unit =
    def use(n: Name) =
      writeInst(s"value_t ${genName(n)};")

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

  // String | Long | Double | Boolean | Sym
  def genValue(v: LitValue): String =
    v match
      case s: String  => s"_al_string_new(rt, \"$s\")"
      case i: Long    => s"_al_int_new(rt, ${i}LL)"
      case d: Double  => s"_al_float_new(rt, ${d})"
      case b: Boolean => if b then "_al_true_new(rt)" else "_al_false_new(rt)"
      case Sym(v)     => s"_al_sym_new(rt, \"$v\")"

  def genTempArray(vs: Seq[Name]) =
    vs.zipWithIndex.foreach { case (v, i) =>
      writeInst(s"_tmp_arr[$i] = ${genNameClosed(v)};")
    }

  def genBody(t: Tree)(using queue: List[(Name, Tree)], cEnv: Map[Name, List[Name]]): Unit =
    def continue() =
      queue.foreach { case (cName, cTree) =>
        write(s"${genName(cName)}:")
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
          s"${genName(name)} = _al_closure_new(rt, (void*)${genName(fn)}, ${args.length}, _tmp_arr);"
        )
        genBody(body)

      case p @ Tree.LetP(name, PrimOp.ListNew | PrimOp.TupleNew, args, body) =>
        genTempArray(args)
        writeInst(
          s"${genName(name)} = _al${cleanBuiltin(p.prim.toString)}(rt, ${args.length}, _tmp_arr);"
        )
        genBody(body)

      case Tree.LetP(name, prim, args, body) =>
        writeInst(
          s"${genName(name)} = _al${cleanBuiltin(prim.toString)}(rt${args.map(v => s", ${genNameClosed(v)}").mkString});"
        )
        genBody(body)

      case Tree.AppF(name: Symbol.Global, Symbol.Ret, args) =>
        writeInst(
          s"return ${genName(name)}(rt${args.map(v => s", ${genNameClosed(v)}").mkString});"
        )
        continue()

      case Tree.AppF(name: Symbol.Global, retC, args) =>
        val List(retN) = cEnv(retC)
        writeInst(
          s"${genName(retN)} = ${genName(name)}(rt${args.map(v => s", ${genNameClosed(v)}").mkString});"
        )
        writeInst(s"goto ${genName(retC)};")
        continue()

      case Tree.AppF(fn, Symbol.Ret, args) =>
        genTempArray(args)
        writeInst(
          s"return _al_call(rt, ${genName(fn)}, ${args.length}, _tmp_arr);"
        )
        continue()

      case Tree.AppF(fn, retC, args) =>
        val List(retN) = cEnv(retC)
        genTempArray(args)
        writeInst(
          s"${genName(retN)} = _al_call(rt, ${genName(fn)}, ${args.length}, _tmp_arr);"
        )
        writeInst(s"goto ${genName(retC)};")
        continue()

      case Tree.AppC(Symbol.Ret, List(arg)) =>
        writeInst(s"return ${genName(arg)};")
        continue()

      case Tree.AppC(fn, args) =>
        cEnv(fn).zip(args).foreach { case (dest, src) =>
          writeInst(s"${genName(dest)} = ${genNameClosed(src)};")
        }
        writeInst(s"goto ${genName(fn)};")
        continue()

      case Tree.If(cond, thenC, elseC) =>
        writeInst(s"if (_al_get_bool(rt, ${genNameClosed(cond)})) goto ${genName(thenC)};")
        writeInst(s"else goto ${genName(elseC)};")
        continue()

      case Tree.Raise(value) =>
        writeInst(s"_al_raise(${genNameClosed(value)});")
        continue()

      case Tree.LetF(name, args, value, body) => !!!
