package aoclang

import java.nio.file.{FileSystems, Files, Path}

case class Xcept(value: Value) extends Exception

val SRC = """
  |def fibrec(a, b, n) = {
  |  if n != 0 {
  |    println("fib: " ++ b)
  |    fibrec(b, a + b, n - 1)
  |  }
  |}
  |
  |def fib(k) = 0 |> fibrec(1, k)
  |
  |def main() = {
  |  10 |> fib |> println
  |}
  |""".stripMargin

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

@main
def main(): Unit =
  val stl = Files.readString(FileSystems.getDefault.getPath("stl/stl.al"))
  val src = Source(stl + SRC)
  val lexer = Lexer(src)
  lexer.dump()

  val topLevel = Parser(src).parseTopLevel

  print_tree(topLevel)
  val globals = execTopLevel(topLevel)

  execExpr(Map.empty, globals)(
    Expr.App(
      Expr.Var((Tok.Id("main") withSpan Span(Source("main"), 0, 4)).asInstanceOf[Tok.Id]),
      Nil
    )
  )

enum Value:
  case Fn(ctx: Map[String, Value], args: List[String], body: Expr | (Map[String, Value] => Value))
  case Lit(value: LitKind)

val NONE = Value.Lit(Sym("none"))

def execTopLevel(prog: List[Decl]): Map[String, Value] =
  prog.foldLeft(Map.empty) { (acc, decl) =>
    decl match
      case Decl.Def(name, args, body) =>
        val bodyLong: Expr | (Map[String, Value] => Value) = body match
          case expr: Expr => expr
          case Intrinsic(_) =>
            INTRINSICS(name.span.get.text)

        acc + (name.span.get.text -> Value.Fn(Map.empty, args.map(_.span.text), bodyLong))
  }

def execExpr(scope: Map[String, Value], globals: Map[String, Value])(prog: Expr): Value =
  try
    prog match
      case Expr.App(fn, args) =>
        val fnValue = execExpr(scope, globals)(fn)
        val argValues = args.map(execExpr(scope, globals))
        fnValue match
          case Value.Fn(ctx, argNames, body) =>
            if argNames.length < argValues.length then
              xcept(s"Expected max ${argNames.length} arguments, got ${argValues.length}")
            else if argNames.length > argValues.length then
              val offset = argNames.length - argValues.length
              val (start, populated) = argNames.splitAt(offset)
              val newScope = ctx ++ populated.zip(argValues).toMap

              Value.Fn(newScope, start, body)
            else
              val newScope = ctx ++ argNames.zip(argValues).toMap
              body match
                case body: Expr                          => execExpr(newScope, globals)(body)
                case body: (Map[String, Value] => Value) => body(newScope)

      case Expr.Let(bind, _, rhs, body) =>
        val value = execExpr(scope, globals)(rhs)
        body
          .map(execExpr(scope + (bind.span.text -> value), globals))
          .getOrElse(NONE)

      case Expr.Var(v) => scope.getOrElse(v.span.get.text, globals(v.span.get.text))

      case Expr.Lit(Tok.Lit(lit)) => Value.Lit(lit)

      case Expr.Block(_, expr, _) => expr.map(execExpr(scope, globals)).getOrElse(NONE)

      case Expr.Discard(a, b) =>
        execExpr(scope, globals)(a)
        execExpr(scope, globals)(b)

      case Expr.If(cond, thenBranch, elseBranch) =>
        if execExpr(scope, globals)(cond) == Value.Lit(true) then
          execExpr(scope, globals)(thenBranch)
        else elseBranch.map(elseBranch => execExpr(scope, globals)(elseBranch)).getOrElse(NONE)

  catch
    case Xcept(value) =>
      prog.span.printLineColumn(s"ERROR: $value")
      System.exit(1)
      throw Error("unreachable")

def xcept(value: String) = throw Xcept(Value.Lit(Sym(value)))

def intrinsicToString(v: Value) =
  v match
    case Value.Lit(v)      => v.toString
    case Value.Fn(_, _, _) => "<fn>"

val INTRINSICS = Map(
  "println" -> { (ctx: Map[String, Value]) =>
    println(intrinsicToString(ctx("a")))
    NONE
  },
  "==" -> { (ctx: Map[String, Value]) => Value.Lit(ctx("a") == ctx("b")) },
  "!=" -> { (ctx: Map[String, Value]) => Value.Lit(ctx("a") != ctx("b")) },
  ">" -> { (ctx: Map[String, Value]) =>
    (ctx("a"), ctx("b")) match
      case (Value.Lit(x: Long), Value.Lit(y: Long))     => Value.Lit(x > y)
      case (Value.Lit(x: Float), Value.Lit(y: Float))   => Value.Lit(x > y)
      case (Value.Lit(x: Long), Value.Lit(y: Float))    => Value.Lit(x > y)
      case (Value.Lit(x: Float), Value.Lit(y: Long))    => Value.Lit(x > y)
      case (Value.Lit(x: String), Value.Lit(y: String)) => Value.Lit(x > y)
      case _                                            => xcept("Invalid types for (>)")
  },
  "<" -> { (ctx: Map[String, Value]) =>
    (ctx("a"), ctx("b")) match
      case (Value.Lit(x: Long), Value.Lit(y: Long))     => Value.Lit(x < y)
      case (Value.Lit(x: Float), Value.Lit(y: Float))   => Value.Lit(x < y)
      case (Value.Lit(x: Long), Value.Lit(y: Float))    => Value.Lit(x < y)
      case (Value.Lit(x: Float), Value.Lit(y: Long))    => Value.Lit(x < y)
      case (Value.Lit(x: String), Value.Lit(y: String)) => Value.Lit(x < y)
      case _                                            => xcept("Invalid types for (<)")
  },
  ">=" -> { (ctx: Map[String, Value]) =>
    (ctx("a"), ctx("b")) match
      case (Value.Lit(x: Long), Value.Lit(y: Long))     => Value.Lit(x >= y)
      case (Value.Lit(x: Float), Value.Lit(y: Float))   => Value.Lit(x >= y)
      case (Value.Lit(x: Long), Value.Lit(y: Float))    => Value.Lit(x >= y)
      case (Value.Lit(x: Float), Value.Lit(y: Long))    => Value.Lit(x >= y)
      case (Value.Lit(x: String), Value.Lit(y: String)) => Value.Lit(x >= y)
      case _                                            => xcept("Invalid types for (>=)")
  },
  "<=" -> { (ctx: Map[String, Value]) =>
    (ctx("a"), ctx("b")) match
      case (Value.Lit(x: Long), Value.Lit(y: Long))     => Value.Lit(x <= y)
      case (Value.Lit(x: Float), Value.Lit(y: Float))   => Value.Lit(x <= y)
      case (Value.Lit(x: Long), Value.Lit(y: Float))    => Value.Lit(x <= y)
      case (Value.Lit(x: Float), Value.Lit(y: Long))    => Value.Lit(x <= y)
      case (Value.Lit(x: String), Value.Lit(y: String)) => Value.Lit(x <= y)
      case _                                            => xcept("Invalid types for (<=)")
  },
  "+" -> { (ctx: Map[String, Value]) =>
    (ctx("a"), ctx("b")) match
      case (Value.Lit(x: Long), Value.Lit(y: Long))   => Value.Lit(x + y)
      case (Value.Lit(x: Float), Value.Lit(y: Float)) => Value.Lit(x + y)
      case (Value.Lit(x: Long), Value.Lit(y: Float))  => Value.Lit(x + y)
      case (Value.Lit(x: Float), Value.Lit(y: Long))  => Value.Lit(x + y)
      case (a, b)                                     => xcept(s"Invalid types for (+): $a $b")
  },
  "-" -> { (ctx: Map[String, Value]) =>
    (ctx("a"), ctx("b")) match
      case (Value.Lit(x: Long), Value.Lit(y: Long))   => Value.Lit(x - y)
      case (Value.Lit(x: Float), Value.Lit(y: Float)) => Value.Lit(x - y)
      case (Value.Lit(x: Long), Value.Lit(y: Float))  => Value.Lit(x - y)
      case (Value.Lit(x: Float), Value.Lit(y: Long))  => Value.Lit(x - y)
      case _                                          => xcept("Invalid types for (-)")
  },
  "*" -> { (ctx: Map[String, Value]) =>
    (ctx("a"), ctx("b")) match
      case (Value.Lit(x: Long), Value.Lit(y: Long))   => Value.Lit(x * y)
      case (Value.Lit(x: Float), Value.Lit(y: Float)) => Value.Lit(x * y)
      case (Value.Lit(x: Long), Value.Lit(y: Float))  => Value.Lit(x * y)
      case (Value.Lit(x: Float), Value.Lit(y: Long))  => Value.Lit(x * y)
      case _                                          => xcept("Invalid types for (*)")
  },
  "**" -> { (ctx: Map[String, Value]) =>
    (ctx("a"), ctx("b")) match
      case (Value.Lit(x: Long), Value.Lit(y: Long))   => Value.Lit(Math.pow(x, y).toLong)
      case (Value.Lit(x: Float), Value.Lit(y: Float)) => Value.Lit(Math.pow(x, y).toLong)
      case (Value.Lit(x: Long), Value.Lit(y: Float))  => Value.Lit(Math.pow(x, y).toLong)
      case (Value.Lit(x: Float), Value.Lit(y: Long))  => Value.Lit(Math.pow(x, y).toLong)
      case _                                          => xcept("Invalid types for (**)")
  },
  "&" -> { (ctx: Map[String, Value]) =>
    (ctx("a"), ctx("b")) match
      case (Value.Lit(x: Long), Value.Lit(y: Long)) => Value.Lit(x & y)
      case _                                        => xcept("Invalid types for (&)")
  },
  "|" -> { (ctx: Map[String, Value]) =>
    (ctx("a"), ctx("b")) match
      case (Value.Lit(x: Long), Value.Lit(y: Long)) => Value.Lit(x | y)
      case _                                        => xcept("Invalid types for (|)")
  },
  "^" -> { (ctx: Map[String, Value]) =>
    (ctx("a"), ctx("b")) match
      case (Value.Lit(x: Long), Value.Lit(y: Long)) => Value.Lit(x ^ y)
      case _                                        => xcept("Invalid types for (^)")
  },
  ">>" -> { (ctx: Map[String, Value]) =>
    (ctx("a"), ctx("b")) match
      case (Value.Lit(x: Long), Value.Lit(y: Long)) => Value.Lit(x >> y)
      case _                                        => xcept("Invalid types for (>>)")
  },
  "<<" -> { (ctx: Map[String, Value]) =>
    (ctx("a"), ctx("b")) match
      case (Value.Lit(x: Long), Value.Lit(y: Long)) => Value.Lit(x << y)
      case _                                        => xcept("Invalid types for (<<)")
  },
  "/%" -> { (ctx: Map[String, Value]) =>
    (ctx("a"), ctx("b")) match
      case (Value.Lit(x: Long), Value.Lit(y: Long)) => Value.Lit(x / y)
      case _                                        => xcept("Invalid types for (/%)")
  },
  "/" -> { (ctx: Map[String, Value]) =>
    (ctx("a"), ctx("b")) match
      case (Value.Lit(x: Long), Value.Lit(y: Long))   => Value.Lit(x.toDouble / y.toDouble)
      case (Value.Lit(x: Float), Value.Lit(y: Float)) => Value.Lit(x / y)
      case (Value.Lit(x: Long), Value.Lit(y: Float))  => Value.Lit(x.toDouble / y)
      case (Value.Lit(x: Float), Value.Lit(y: Long))  => Value.Lit(x / y.toDouble)
      case _                                          => xcept("Invalid types for (/)")
  },
  "%" -> { (ctx: Map[String, Value]) =>
    (ctx("a"), ctx("b")) match
      case (Value.Lit(x: Long), Value.Lit(y: Long)) => Value.Lit(x % y)
      case _                                        => xcept("Invalid types for (%)")
  },
  "++" -> { (ctx: Map[String, Value]) =>
    (ctx("a"), ctx("b")) match
      case (Value.Lit(x), Value.Lit(y)) => Value.Lit(s"$x$y")
      case (a, b)                       => xcept(s"Invalid types for (+): $a $b")
  }
)
