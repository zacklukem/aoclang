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

  val mainfn = "Lists" :@: "main"

  val LowDecl.Def(_, main) = interp.decls(mainfn)

  val start = System.nanoTime
  try interp.eval(main)(using Map.empty, List(mainfn))
  catch
    case XceptWithStack(msg, stack) =>
      println(s"ERROR: $msg")
      stack.foreach { frame =>
        println(s"\t$frame")
      }
  val time = (System.nanoTime - start) / 1e6

  println(s"Execution time: $time ms")
