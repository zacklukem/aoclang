package aoclang

import scala.jdk.CollectionConverters.*
import java.nio.file.{Files, Path}

def print_tree(obj: Any, depth: Int = 0, paramName: Option[String] = None): Unit =
  val indent = "| " * depth
  val prettyName = paramName.fold("")(x => s"$x: ")
  val ptype = obj match
    case _: Iterable[Any] => ""
    case obj: Product     => obj.productPrefix
    case _                => obj.toString

  println(s"$indent$prettyName$ptype")

  obj match
    case seq: Iterable[Any] =>
      seq.foreach(print_tree(_, depth + 1))
    case obj: Product =>
      if obj.productArity == 1 then print_tree(obj.productIterator.next, depth + 1, None)
      else
        (obj.productIterator zip obj.productElementNames)
          .foreach { case (subObj, paramName) => print_tree(subObj, depth + 1, Some(paramName)) }
    case _ =>

def pascalCase(s: String) =
  s.split("_").map(_.capitalize).mkString

def isTest(s: Symbol) =
  s match
    case Symbol.Global(List(mod, fn)) => mod.startsWith("Test") && fn.startsWith("test_")
    case _                            => false

@main
def main(): Unit =
  val stl_root = Path.of("stl")
  val examples = Path.of("examples")
  val files =
    (Files.walk(stl_root).iterator().asScala ++ Files.walk(examples).iterator().asScala)
      .filter(_.toString.endsWith(".al"))
      .toList

  val moduleAsts = files.map { path =>
    val src = Files.readString(path)
    val tree = Parser(Source(src)).parseTopLevel
    val modname = path.getFileName.toString.stripSuffix(".al") |> pascalCase
    modname -> tree
  }

  val lower = Lower()
  moduleAsts.foreach({ case (mod, decls) => lower.declare(mod, decls) })
  moduleAsts.foreach({ case (mod, decls) => lower.lower(mod, decls) })

  val interp = Interp(lower.decls.toMap)

  lower.decls.foreach { case (name, decl) =>
    if isTest(name) then
      print(s"\u001b[34mTEST $name... \u001b[0m".padTo(60, ' '))
      val LowDecl.Def(_, body) = decl
      try
        val start = System.nanoTime
        interp.eval(body)(using Map.empty, List(name))
        val time = (System.nanoTime - start) / 1e6
        println(s"\u001b[32mPASS ($time ms)\u001b[0m")
      catch
        case XceptWithStack(msg, stack) =>
          println(s"\n\u001b[31m\tERROR: $msg\u001b[0m")
          stack.foreach { frame =>
            println(s"\u001b[31m\t\t$frame\u001b[0m")
          }
  }
