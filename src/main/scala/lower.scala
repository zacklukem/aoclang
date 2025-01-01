package aoclang

import scala.collection.mutable;

enum Symbol:
  case Local(id: String)
  case Global(name: List[String])
  case Ret

  override def toString: String =
    this match
      case Symbol.Local(id)    => id
      case Symbol.Global(name) => s"${name.mkString(".")}"
      case Ret                 => "@ret"

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
  case LetF(name: Symbol, args: List[Symbol], value: Tree, body: Tree)
  case LetL(name: Symbol, value: LitValue, body: Tree)
  case If(cond: Symbol, thenC: Symbol, elseC: Symbol)
  case Raise(value: Symbol)

  def pretty(depth: Int = 0): Unit =
    def line(s: String) = println("  " * depth + s)

    this match
      case Tree.AppF(fn, retC, args) =>
        line(s"appf ${fn}(${args.mkString(",")}) -> ${retC}")
      case Tree.AppC(fn, args) =>
        line(s"appc ${fn}(${args.mkString(",")})")
      case Tree.LetC(name, args, value, body) =>
        line(s"letc ${name}(${args.mkString(",")}) = {")
        value.pretty(depth + 1)
        line("}")
        body.pretty(depth)
      case LetF(name, args, value, body) =>
        line(s"letf ${name}(${args.mkString(",")}) = {")
        value.pretty(depth + 1)
        line("}")
        body.pretty(depth)
      case Tree.LetL(name, value, body) =>
        line(s"letl ${name} = ${value}")
        body.pretty(depth)
      case Tree.If(cond, thenC, elseC) =>
        line(s"if ${cond} then ${thenC} else ${elseC}")
      case Tree.Raise(value) =>
        line(s"raise ${value}")

enum LowDecl:
  case Def(args: List[Symbol], body: Tree)
  case Intrinsic(name: String)

  def pretty(): Unit =
    this match
      case LowDecl.Def(args, body) =>
        println(s"def ${args.mkString(",")} = {")
        body.pretty(1)
        println("}")
      case LowDecl.Intrinsic(name) =>
        println(s"intrinsic ${name}")

extension (s: String) def :@:(b: String): Symbol = Symbol.Global(List(s, b))

private def letl(value: LitValue)(body: Symbol => Tree): Tree =
  val sym = Symbol.local
  Tree.LetL(sym, value, body(sym))

private def letlNone(body: Symbol => Tree): Tree =
  letl(Sym.none)(body)

private def letc(args: List[Symbol], value: Tree)(body: Symbol => Tree): Tree =
  val sym = Symbol.local
  Tree.LetC(sym, args, value, body(sym))

private def app(fn: Symbol, args: List[Symbol])(thenC: Symbol => Tree): Tree =
  val ret = Symbol.local
  letc(List(ret), thenC(ret)) { retC =>
    Tree.AppF(fn, retC, args)
  }

private def iff(cond: Symbol)(thenT: => Tree)(elseT: => Tree): Tree =
  letc(List(), thenT) { thenC =>
    letc(List(), elseT) { elseC =>
      Tree.If(cond, thenC, elseC)
    }
  }

private def lower_pat_product(p: Seq[(Pat, Symbol)])(
    c: Option[Map[Tok.Id | Tok.Op, Symbol]] => Tree
): Tree =
  p match
    case Seq() => c(Some(Map.empty))
    case (p, rhs) +: rest =>
      lower_pat(p, rhs) {
        case Some(bindings) =>
          lower_pat_product(rest)({ v => c(v.map(_ ++ bindings)) })
        case None => c(None)
      }

def applyAll[T](x: Seq[T])(f: (T, Symbol => Tree) => Tree)(c: List[Symbol] => Tree): Tree =
  x match
    case Seq() => c(Nil)
    case x +: rest =>
      f(
        x,
        { value =>
          applyAll(rest)(f)(values => c(value :: values))
        }
      )

private def lower_pat(p: Pat, rhs: Symbol)(c: Option[Map[Tok.Id | Tok.Op, Symbol]] => Tree): Tree =
  p match
    case Pat.Bind(binding) =>
      c(Some(Map(binding -> rhs)))

    case Pat.Lit(Tok.Lit(lit)) =>
      letl(lit) { lit =>
        app("Stl" :@: "==", List(rhs, lit)) { is_eq =>
          iff(is_eq) {
            c(Some(Map.empty))
          } { c(None) }
        }
      }

    case Pat.Tuple(_, pats, _) =>
      app("Tuple" :@: "is", List(rhs)) { is_tuple =>
        iff(is_tuple) {
          app("Tuple" :@: "size", List(rhs)) { size =>
            letl(pats.size.toLong) { expected_size =>
              app("Stl" :@: "==", List(size, expected_size)) { is_eq =>
                iff(is_eq) {
                  applyAll(pats.indices) { (idx, c) =>
                    letl(idx.toLong) { idx =>
                      app("Tuple" :@: "get", List(rhs, idx)) { value =>
                        c(value)
                      }
                    }
                  } { values =>
                    lower_pat_product(pats.zip(values))(c)
                  }
                } {
                  c(None)
                }
              }
            }
          }
        } { c(None) }
      }

    case Pat.Cons(head, tail) =>
      app("List" :@: "is", List(rhs)) { is_list =>
        iff(is_list) {
          app("List" :@: "head", List(rhs)) { headVal =>
            letl(Sym.none) { nonel =>
              app("Stl" :@: "!=", List(headVal, nonel)) { head_is_some =>
                iff(head_is_some) {
                  lower_pat(head, headVal) {
                    case Some(head_bindings) =>
                      app("List" :@: "tail", List(rhs)) { tailVal =>
                        lower_pat(tail, tailVal) {
                          case Some(tail_bindings) => c(Some(head_bindings ++ tail_bindings))
                          case None                => c(None)
                        }
                      }
                    case None => c(None)
                  }
                } { c(None) }
              }
            }
          }
        } { c(None) }
      }

    case Pat.ListLit(_, Nil, _) =>
      app("List" :@: "is", List(rhs)) { is_list =>
        iff(is_list) {
          app("List" :@: "is_empty", List(rhs)) { is_empty =>
            iff(is_empty) {
              c(Some(Map.empty))
            } { c(None) }
          }
        } { c(None) }
      }

    case Pat.TypeAssert(pat, Tok.Id(ty)) =>
      app(ty :@: "is", List(rhs)) { is_of_ty =>
        iff(is_of_ty) {
          lower_pat(pat, rhs)(c)
        } { c(None) }
      }

    case Pat.ListLit(_, pats, _) =>
      def reduce_pats(
          pats: List[Pat],
          prev: Symbol,
          bindings: Map[Tok.Id | Tok.Op, Symbol]
      ): Tree =
        pats match
          case Nil =>
            c(Some(bindings))
          case pat :: tail =>
            app("List" :@: "head", List(prev)) { head =>
              lower_pat(pat, head) {
                case Some(head_bindings) =>
                  app("List" :@: "tail", List(prev)) { tailVal =>
                    reduce_pats(tail, tailVal, bindings ++ head_bindings)
                  }
                case None => c(None)
              }
            }

      app("List" :@: "is", List(rhs)) { is_list =>
        iff(is_list) {
          reduce_pats(pats, rhs, Map.empty)
        } { c(None) }
      }

def emit(span: Span, e: String) =
  span.printLineColumn("error")
  throw new Exception(e)

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
    val globals = modules("List") ++ modules("Stl") ++ modules(mod)

    decl
      .groupBy { case Decl.Def(name, _, _) => name.span.get.text }
      .foreach { case (name, decl) =>
        decl.head match
          case Decl.Def(_, _, Intrinsic(_)) =>
            assert(decl.size == 1)

            val symbol = modules(mod)(name)
            decls(symbol) = LowDecl.Intrinsic(s"$mod.$name")
          case Decl.Def(_, args, _) =>
            val arity = args.length
            val symbol = modules(mod)(name)

            assert(decl.forall { case Decl.Def(_, declArgs, _) => declArgs.length == arity })

            decls(symbol) = LowerExpr(globals, modules.toMap).lowerDeclGroup(decl, arity)
      }

class LowerExpr(
    val globals: Map[String, Symbol],
    val modules: Map[String, Map[String, Symbol]]
):
  def lowerDeclGroup(decls: List[Decl], arity: Int): LowDecl =
    val argsSym = (1 to arity).map { idx => Symbol.local(s"_arg${idx}_") }.toList

    val fail = letl("match error")(Tree.Raise(_))
    val e = decls.foldRight(fail) { case (Decl.Def(name, pat, body: Expr), next) =>
      lower_pat_product(pat.zip(argsSym)) {
        case Some(bindings) =>
          lower_tail(body)(Symbol.Ret)(using bindings)
        case None =>
          next
      }
    }

    LowDecl.Def(argsSym, e)

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
      case Expr.Let(pat, rhs, body) =>
        lower(rhs) { value =>
          lower_pat(pat, value) {
            case Some(bindings) =>
              lower(body)(c)(using sym ++ bindings)
            case None =>
              letl("match error")(Tree.Raise(_))
          }
        }

      case Expr.App(fn, args) =>
        lower(fn) { fn =>
          lower(args) { args =>
            val res = Symbol.local
            letc(List(res), c(res))(Tree.AppF(fn, _, args))
          }
        }

      case Expr.Lit(Tok.Lit(value)) =>
        letl(value)(c)

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
          val res = Symbol.local
          letc(List(res), c(res)) { c =>
            letc(Nil, lower_tail(thenE)(c)) { thenC =>
              letc(Nil, lower_tail(elseE)(c)) { elseC =>
                Tree.If(cond, thenC, elseC)
              }
            }
          }
        }

      case Expr.Field(Expr.Var(Tok.Id(name)), Tok.Id(field)) =>
        c(modules(name)(field))

      case Expr.Var(name) =>
        sym.get(name).orElse(globals.get(name.span.get.text)) match
          case Some(s) => c(s)
          case None    => emit(name.span.get, s"undefined variable ${name.span.get.text}")

      case Expr.Tuple(_, elems, _) =>
        lower(elems) { args =>
          val res = Symbol.local
          letc(List(res), c(res))(Tree.AppF("Tuple" :@: "new", _, args))
        }

      case Expr.ListLit(_, elems, _) =>
        lower(elems) { args =>
          val res = Symbol.local
          letc(List(res), c(res))(Tree.AppF("List" :@: "new", _, args))
        }

      case Expr.TupleField(tup, Tok.Lit(idx: Long)) =>
        lower(tup) { tup =>
          val res = Symbol.local
          letl(idx) { idx =>
            letc(List(res), c(res))(
              Tree.AppF("Tuple" :@: "get", _, List(tup, idx))
            )
          }
        }

      case Expr.Match(expr, _, cases, _) =>
        val res = Symbol.local
        letc(List(res), c(res)) { c =>
          lower(expr) { expr =>
            val fail = letl("match error")(Tree.Raise(_))
            cases.foldRight(fail) { case (MatchCase(pat, body), next) =>
              lower_pat(pat, expr) {
                case Some(bindings) =>
                  lower_tail(body)(c)(using sym ++ bindings)
                case None =>
                  next
              }
            }
          }
        }

      case Expr.Lambda(_, args, _, body) =>
        val name = Symbol.local
        val argSyms = args.map { _ => Symbol.local }
        Tree.LetF(
          name,
          argSyms,
          lower_pat_product(args.zip(argSyms)) {
            case Some(bindings) =>
              lower_tail(body)(Symbol.Ret)(using sym ++ bindings)
            case None =>
              letl("match error")(Tree.Raise(_))
          },
          c(name)
        )

  def lower_tail(e: Option[Expr])(c: Symbol)(using sym: Map[Tok.Id | Tok.Op, Symbol]): Tree =
    e
      .map(lower_tail(_)(c))
      .getOrElse(letlNone(s => Tree.AppC(c, List(s))))

  def lower_tail(e: Expr)(c: Symbol)(using sym: Map[Tok.Id | Tok.Op, Symbol]): Tree =
    def cf(s: Symbol) = Tree.AppC(c, List(s))

    e match
      case Expr.Let(pat, rhs, body) =>
        lower(rhs) { value =>
          lower_pat(pat, value) {
            case Some(bindings) =>
              lower_tail(body)(c)(using sym ++ bindings)
            case None =>
              letl("match error")(Tree.Raise(_))
          }
        }

      case Expr.App(fn, args) =>
        lower(fn) { fn =>
          lower(args) { args =>
            Tree.AppF(fn, c, args)
          }
        }

      case Expr.Lit(Tok.Lit(value)) =>
        letl(value)(cf)

      case Expr.Block(_, body, _) =>
        lower_tail(body)(c)

      case Expr.Discard(e, body) =>
        lower(e) { _ =>
          lower_tail(body)(c)
        }

      case Expr.If(cond, thenE, elseE) =>
        lower(cond) { cond =>
          val thenSym = Symbol.local
          val elseSym = Symbol.local
          letc(Nil, lower_tail(thenE)(c)) { thenC =>
            letc(Nil, lower_tail(elseE)(c)) { elseC =>
              Tree.If(cond, thenC, elseC)
            }
          }
        }

      case Expr.Field(Expr.Var(Tok.Id(name)), Tok.Id(field)) =>
        cf(modules(name)(field))

      case Expr.Var(name) =>
        cf(sym.getOrElse(name, globals(name.span.get.text)))

      case Expr.Tuple(_, elems, _) =>
        lower(elems) { args =>
          Tree.AppF("Tuple" :@: "new", c, args)
        }

      case Expr.TupleField(tup, Tok.Lit(idx: Long)) =>
        lower(tup) { tup =>
          letl(idx) { idx =>
            Tree.AppF("Tuple" :@: "get", c, List(tup, idx))
          }
        }

      case Expr.ListLit(_, elems, _) =>
        lower(elems) { args =>
          Tree.AppF("List" :@: "new", c, args)
        }

      case Expr.Match(expr, _, cases, _) =>
        lower(expr) { expr =>
          val fail = letl("match error")(Tree.Raise(_))
          cases.foldRight(fail) { case (MatchCase(pat, body), next) =>
            lower_pat(pat, expr) {
              case Some(bindings) =>
                lower_tail(body)(c)(using sym ++ bindings)
              case None =>
                next
            }
          }
        }

      case Expr.Lambda(_, args, _, body) =>
        val name = Symbol.local
        val argSyms = args.map { _ => Symbol.local }
        Tree.LetF(
          name,
          argSyms,
          lower_pat_product(args.zip(argSyms)) {
            case Some(bindings) =>
              lower_tail(body)(Symbol.Ret)(using sym ++ bindings)
            case None =>
              letl("match error")(Tree.Raise(_))
          },
          cf(name)
        )
