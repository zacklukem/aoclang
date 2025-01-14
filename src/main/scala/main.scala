package aoclang

import scopt.OParser

import scala.jdk.CollectionConverters.*
import java.nio.file.{Files, Path}
import java.io.{File, PrintWriter}
import sys.process.*

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

def trackTime[A, B](label: String, f: A => B)(using opt: Config): A => B =
  x =>
    val start = System.nanoTime
    val res = f(x)
    val time = (System.nanoTime - start) / 1e6
    if opt.print_times then println(s"$label: ${"%.2f".format(time)} ms")
    res

def load(files: List[Path])(using opt: Config) =
  files.map { path =>
    val src = Files.readString(path)
    val modname = path.getFileName.toString.stripSuffix(".al") |> pascalCase
    modname -> Source(src)
  }

def parse(files: List[(String, Source)])(using opt: Config) =
  files.map { case (modname, src) =>
    modname -> Parser(src).parseTopLevel
  }

def interpretTests(decls: Map[Symbol, Low.Decl])(using opt: Config): Unit =
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

def build(files: List[Path])(using
    opt: Config
): (Map[Symbol, Low.Decl], Map[String, Map[String, Symbol]]) =
  val ast = files
    |> trackTime("load", load)
    |> trackTime("parse", parse)
  val (decls, modules) = trackTime("lower", lower)(ast)
  val code = decls
    |> trackTime("hoist", hoist)
    |> trackTime("optimize", optimize)
    |> trackTime("regalloc", regalloc)
  (code, modules)

def loadFiles()(using opt: Config) =
  val stl_root = Path.of(opt.aoc_home, "stl", "src")
  var dirs = List(Path.of(".", "src"))

  if opt.include_stl then dirs = stl_root :: dirs

  if opt.mode == "test" || opt.mode == "compile" then dirs = Path.of(".", "test") :: dirs

  val files =
    dirs
      .flatMap(Files.walk(_).iterator().asScala)
      .filter(_.toString.endsWith(".al"))

  files

def compileCmd()(using opt: Config): Unit =
  def tryx(x: ProcessBuilder) =
    val log = ProcessLogger(System.out.println, System.err.println)
    if x.!(log) != 0 then
      System.err.println("PROCESS FAILED")
      System.out.flush()
      System.err.flush()
      System.exit(1)
  val files = loadFiles()
  val (decls, _) = build(files)
  Files.deleteIfExists(Path.of("out.c"))
  val cfile = Files.createFile(Path.of("out.c")).toFile
  val w = PrintWriter(cfile)
  Gen(w).genAll(decls)
  println("Building runtime...")
  tryx(Process("cargo build --release", File(opt.aoc_home, "rt")))
  println("Building c file...")
  tryx(
    s"cc -g -O3 ${cfile.getAbsolutePath} ${opt.aoc_home}/rt/target/release/librt.a -I${opt.aoc_home}/rt/include -o a.out"
  )
  println("Running executable...")
  tryx(s"./a.out")

def testCmd()(using opt: Config): Unit =
  val files = loadFiles()
  val (decls, _) = build(files)
  interpretTests(decls)

def runCmd()(using opt: Config): Unit =
  val files = loadFiles()
  val (decls, _) = build(files)
  val interp = Interp(decls)
  val Array(a, b) = opt.function.split('.')
  val declName = a :@: b

  decls(declName) match
    case decl @ Low.Decl.Def(_, body) =>
      try interp.eval(body)(using Array.ofDim(decl.maxStack.get), List(declName))
      catch
        case XceptWithStack(msg, stack) =>
          println(s"\n\u001b[31m\tERROR: $msg\u001b[0m")
          stack.foreach { frame =>
            println(s"\u001b[31m\t\t$frame\u001b[0m")
          }
    case _ =>
      println(s"Function $declName not found")

def replCmd()(using opt: Config): Unit =
  val files = loadFiles()
  val (decls, modules) = build(files)
  val interp = Interp(decls)
  val initScope = Array.ofDim[Value](1000)

  while true do
    print("> ")
    val line = scala.io.StdIn.readLine()
    val src = Source(line)
    val parser = Parser(src)
    val expr = parser.parseSingleExpr
    val tree = LowerExpr(modules("Stl"), modules).lower(expr) { v =>
      High.Tree.AppC(Symbol.Ret, List(v))
    }(using Map.empty)
    val hoist = Hoist()
    val hTree = hoist.hoist(tree)(using Map.empty)
    val lowTree = regalloc(hTree)
    interp.decls ++= hoist.newDecls.toList.map { case (sym, decl) => sym -> regalloc(decl) }
    val res = interp.eval(lowTree)(using initScope, List(Symbol.Global(List("Repl", "repl"))))
    println(intrinsicToString(res))
    if line == null then return

case class Config(
    mode: String = "repl",
    function: String = "",
    aoc_home: String = System.getenv("AOCLANG_HOME"),
    include_stl: Boolean = true,
    print_times: Boolean = false
)

val builder = OParser.builder[Config]
val argparse =
  import builder.*
  OParser.sequence(
    programName("aoclang"),
    cmd("run")
      .action((_, c) => c.copy(mode = "run"))
      .children(
        arg[String]("function")
          .action((x, c) => c.copy(mode = "run", function = x))
          .text("function to run")
      ),
    cmd("test")
      .action((_, c) => c.copy(mode = "test")),
    cmd("compile")
      .action((_, c) => c.copy(mode = "compile")),
    opt[Unit]("no-stl")
      .action((_, c) => c.copy(include_stl = false)),
    opt[Unit]("times")
      .action((_, c) => c.copy(print_times = true))
  )

@main
def main(args: String*): Unit =
  OParser.parse(argparse, args, Config()) match
    case Some(opt) =>
      opt.mode match
        case "test"    => testCmd()(using opt)
        case "run"     => runCmd()(using opt)
        case "repl"    => replCmd()(using opt)
        case "compile" => compileCmd()(using opt)

    case None =>
