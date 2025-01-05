package aoclang

import scala.jdk.CollectionConverters.*
import java.nio.file.{Files, Path}

def printTree(obj: Any, depth: Int = 0, paramName: Option[String] = None): Unit =
  val indent = "| " * depth
  val prettyName = paramName.fold("")(x => s"$x: ")
  val ptype = obj match
    case _: Iterable[Any] => ""
    case obj: Product     => obj.productPrefix
    case _                => obj.toString

  println(s"$indent$prettyName$ptype")

  obj match
    case seq: Iterable[Any] =>
      seq.foreach(printTree(_, depth + 1))
    case obj: Product =>
      if obj.productArity == 1 then printTree(obj.productIterator.next, depth + 1, None)
      else
        (obj.productIterator zip obj.productElementNames)
          .foreach { case (subObj, paramName) => printTree(subObj, depth + 1, Some(paramName)) }
    case _ =>

def pascalCase(s: String) =
  s.split("_").map(_.capitalize).mkString

def isTest(s: Symbol) =
  s match
    case Symbol.Global(List(mod, fn)) => mod.startsWith("Test") && fn.startsWith("test_")
    case _                            => false

def dumpAll(decls: Map[Symbol, High.Decl]): Unit =
  decls.foreach { case (name, decl) =>
    if name == "Rand" :@: "next" then decl.pretty(name)
  }

def trackTime[A, B](label: String, f: A => B): A => B =
  x =>
    val start = System.nanoTime
    val res = f(x)
    val time = (System.nanoTime - start) / 1e6
    println(s"$label: ${"%.2f".format(time)} ms")
    res

def load(files: List[Path]) =
  files.map { path =>
    val src = Files.readString(path)
    val modname = path.getFileName.toString.stripSuffix(".al") |> pascalCase
    modname -> Source(src)
  }

def parse(files: List[(String, Source)]) =
  files.map { case (modname, src) =>
    modname -> Parser(src).parseTopLevel
  }

@main
def main(): Unit =
  val stl_root = Path.of("stl")
  val examples = Path.of("examples")
  val files =
    (Files.walk(stl_root).iterator().asScala ++ Files.walk(examples).iterator().asScala)
      .filter(_.toString.endsWith(".al"))
      .toList

  val decls =
    files
      |> trackTime("load", load)
      |> trackTime("parse", parse)
      |> trackTime("lower", lower)
      |> trackTime("hoist", hoist)
      |> trackTime("optimize", optimize)
      |> trackTime("regalloc", regalloc)

  val interp = Interp(decls)

  decls.toSeq
    .filter((name, _) => isTest(name))
    .sortBy(_._1.toString)
    .foreach { case (name, decl) =>
      print(s"\u001b[34mTEST $name... \u001b[0m".padTo(60, ' '))
      val Low.Decl.Def(_, body) = decl
      try
        val start = System.nanoTime
        interp.eval(body)(using Array.ofDim(decl.maxStack.get), List(name))
        val time = (System.nanoTime - start) / 1e6
        println(s"\u001b[32mPASS ${"%8.3f".format(time)} ms\u001b[0m")
      catch
        case XceptWithStack(msg, stack) =>
          println(s"\n\u001b[31m\tERROR: $msg\u001b[0m")
          stack.foreach { frame =>
            println(s"\u001b[31m\t\t$frame\u001b[0m")
          }
    }
