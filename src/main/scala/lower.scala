package aoclang

import scala.collection.mutable;

enum Symbol:
  case Local(id: String)
  case Global(name: List[String])

object Symbol:
  var idx = 0
  def local: Symbol =
    Symbol.local("_")

  def local(name: String): Symbol =
    idx += 1
    Local(s"$name$idx")

enum Tree:
  case AppF(fn: Symbol, retC: Symbol, args: List[Symbol])
  case AppC(fn: Symbol, args: List[Symbol])
  case LetC(name: Symbol, args: List[Symbol], value: Tree, body: Tree)
  case LetL(name: Symbol, value: LitValue, body: Tree)
  case If(cond: Symbol, thenC: Symbol, elseC: Symbol)
  case Ret(value: Symbol)

enum LowDecl:
  case Def(args: List[Symbol], body: Tree)
  case Intrinsic(name: String)

class Lower:
  val modules: mutable.Map[String, Map[String, Symbol]] = mutable.Map()
  val decls: mutable.Map[Symbol, LowDecl] = mutable.Map()

  def declare(mod: String, decl: List[Decl]): Unit =
    decl.foreach(declare(mod, _))

  def declare(mod: String, decl: Decl): Unit =
    decl match
      case Decl.Def(name, _, _) =>
        val symbol = Symbol.Global(List(mod, name.span.get.text))
        modules.updateWith(mod) {
          case None      => Some(Map(name.span.get.text -> symbol))
          case Some(map) => Some(map + (name.span.get.text -> symbol))
        }

  def lower(mod: String, decl: List[Decl]): Unit =
    decl.foreach(lower(mod, _))

  def lower(mod: String, decl: Decl): Unit =
    decl match
      case Decl.Def(name, args, body) =>
        val symbol = modules(mod)(name.span.get.text)
        val argsSym = args.map { case Pat.Bind(arg) =>
          Symbol.local(arg.span.get.text)
        }
        val argsBindings = args
          .zip(argsSym)
          .map { case (Pat.Bind(arg), sym) =>
            (arg: Tok.Id | Tok.Op) -> sym
          }
          .toMap

        val lowDecl = body match
          case Intrinsic(_) =>
            LowDecl.Intrinsic(name.span.get.text)
          case body: Expr =>
            val tree =
              LowerExpr(modules("Stl") ++ modules(mod), modules.toMap)
                .lower(body)(Tree.Ret.apply)(using argsBindings)
            LowDecl.Def(argsSym, tree)

        decls(symbol) = lowDecl

class LowerExpr(
    val globals: Map[String, Symbol],
    val modules: Map[String, Map[String, Symbol]]
):
  private def letl(value: LitValue, body: Symbol => Tree, name: String = "_lit"): Tree =
    val sym = Symbol.local(name)
    Tree.LetL(sym, value, body(sym))

  private def letlNone(body: Symbol => Tree): Tree =
    letl(Sym.none, body, "none")

  private def letc(args: List[Symbol], value: Tree)(body: Symbol => Tree): Tree =
    val sym = Symbol.local
    Tree.LetC(sym, args, value, body(sym))

  def lower(e: Option[Expr])(c: Symbol => Tree)(using sym: Map[Tok.Id | Tok.Op, Symbol]): Tree =
    e
      .map(lower(_)(c))
      .getOrElse(letlNone(c))

  def lower(e: Seq[Expr])(c: List[Symbol] => Tree)(using sym: Map[Tok.Id | Tok.Op, Symbol]): Tree =
    e match
      case Seq() => c(List())
      case e +: rest =>
        lower(e) { value =>
          lower(rest)(values => c(value :: values))
        }

  def lower(e: Expr)(c: Symbol => Tree)(using sym: Map[Tok.Id | Tok.Op, Symbol]): Tree =
    e match
      case Expr.Let(Pat.Bind(binding), rhs, body) =>
        lower(rhs) { value =>
          lower(body)(c)(using sym + (binding -> value))
        }

      case Expr.App(fn, args) =>
        lower(fn) { fn =>
          lower(args) { args =>
            val res = Symbol.local
            letc(List(res), c(res))(Tree.AppF(fn, _, args))
          }
        }

      case Expr.Lit(Tok.Lit(value)) =>
        letl(value, c)

      case Expr.Block(_, body, _) =>
        lower(body)(c)

      case Expr.Discard(e, body) =>
        lower(e) { _ =>
          lower(body)(c)
        }

      case Expr.If(cond, thenE, elseE) =>
        lower(cond) { cond =>
          val thenSym = Symbol.local
          val elseSym = Symbol.local
          letc(Nil, lower(thenE)(c)) { thenC =>
            letc(Nil, lower(elseE)(c)) { elseC =>
              Tree.If(cond, thenC, elseC)
            }
          }
        }

      case Expr.Field(Expr.Var(Tok.Id(name)), Tok.Id(field)) =>
        c(modules(name)(field))

      case Expr.Var(name) =>
        c(sym.getOrElse(name, globals(name.span.get.text)))
