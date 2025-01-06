package aoclang

import High.{Tree, app, iff, letc, letl, letlNone, letp}

import scala.annotation.targetName
import scala.collection.mutable

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

enum PrimOp:
  case PrintLine, Assert, AssertEq, HashCode
  case Ref, Store, Load
  case Eq, Neq, Lt, Gt, Le, Ge, Add, Sub, Mul, DivInt, DivFloat, Mod, Pow, BAnd, BOr, Xor, Shl, Shr,
    Concat, Not
  case ListNew, ListHead, ListTail, ListIs, ListIsEmpty, ListToTuple, ListCons
  case TupleNew, TupleGet, TupleIs, TupleSize, TuplePut
  case StringChars, StringSize, StringFromChars, StringSplit, StringFromInt
  case IntIs, IntFromString
  case ClosureNew

extension (s: String)
  @targetName("makeSymbol")
  def :@:(b: String): Symbol = Symbol.Global(List(s, b))

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

def applyAll[T](x: Seq[T])(f: (T, Symbol => Tree) => Tree)(
    c: List[Symbol] => Tree
): Tree =
  x match
    case Seq() => c(Nil)
    case x +: rest =>
      f(
        x,
        { value =>
          applyAll(rest)(f)(values => c(value :: values))
        }
      )

private def lower_pat(p: Pat, rhs: Symbol)(
    c: Option[Map[Tok.Id | Tok.Op, Symbol]] => Tree
): Tree =
  p match
    case Pat.Bind(binding) =>
      c(Some(Map(binding -> rhs)))

    case Pat.Lit(Tok.Lit(lit)) =>
      letl(lit) { lit =>
        letp(PrimOp.Eq, List(rhs, lit)) { is_eq =>
          iff(is_eq) {
            c(Some(Map.empty))
          } { c(None) }
        }
      }

    case Pat.Tuple(_, pats, _) =>
      letp(PrimOp.TupleIs, List(rhs)) { is_tuple =>
        iff(is_tuple) {
          letp(PrimOp.TupleSize, List(rhs)) { size =>
            letl(pats.size.toLong) { expected_size =>
              letp(PrimOp.Eq, List(size, expected_size)) { is_eq =>
                iff(is_eq) {
                  applyAll(pats.indices) { (idx, c) =>
                    letl(idx.toLong) { idx =>
                      letp(PrimOp.TupleGet, List(rhs, idx)) { value =>
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
      letp(PrimOp.ListIs, List(rhs)) { is_list =>
        iff(is_list) {
          letp(PrimOp.ListHead, List(rhs)) { wrappedHeadVal =>
            letl(Sym.none) { nonel =>
              letp(PrimOp.Neq, List(wrappedHeadVal, nonel)) { head_is_some =>
                iff(head_is_some) {
                  letl(1.toLong) { one =>
                    letp(PrimOp.TupleGet, List(wrappedHeadVal, one)) { headVal =>
                      lower_pat(head, headVal) {
                        case Some(head_bindings) =>
                          letp(PrimOp.ListTail, List(rhs)) { tailVal =>
                            lower_pat(tail, tailVal) {
                              case Some(tail_bindings) => c(Some(head_bindings ++ tail_bindings))
                              case None                => c(None)
                            }
                          }
                        case None => c(None)
                      }
                    }
                  }
                } { c(None) }
              }
            }
          }
        } { c(None) }
      }

    case Pat.ListLit(_, Nil, _) =>
      letp(PrimOp.ListIs, List(rhs)) { is_list =>
        iff(is_list) {
          letp(PrimOp.ListIsEmpty, List(rhs)) { is_empty =>
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
            letp(PrimOp.ListIsEmpty, List(prev)) { is_empty =>
              iff(is_empty) {
                c(Some(bindings))
              } { c(None) }
            }
          case pat :: tail =>
            letp(PrimOp.ListHead, List(prev)) { wrappedHead =>
              letl(Sym.none) { none =>
                letp(PrimOp.Neq, List(wrappedHead, none)) { has_head =>
                  iff(has_head) {
                    letl(1.toLong) { one =>
                      letp(PrimOp.TupleGet, List(wrappedHead, one)) { head =>
                        lower_pat(pat, head) {
                          case Some(head_bindings) =>
                            letp(PrimOp.ListTail, List(prev)) { tailVal =>
                              reduce_pats(tail, tailVal, bindings ++ head_bindings)
                            }
                          case None => c(None)
                        }
                      }
                    }
                  } {
                    c(None)
                  }
                }
              }
            }

      letp(PrimOp.ListIs, List(rhs)) { is_list =>
        iff(is_list) {
          reduce_pats(pats, rhs, Map.empty)
        } { c(None) }
      }

def emit(span: Span, e: String): Nothing =
  span.printLineColumn("error")
  throw new Exception(e)

def lower(
    asts: List[(String, List[Decl])]
): (Map[Symbol, High.Decl], Map[String, Map[String, Symbol]]) =
  val lower = Lower()
  asts.foreach({ case (mod, decls) => lower.declare(mod, decls) })
  asts
    .flatMap({ case (mod, decls) =>
      decls.map {
        case decl: Decl.Def => (mod, mod, decl)
        case Decl.AbsoluteDef(List(Tok.Id(realMod), name), args, body) =>
          (realMod, mod, Decl.Def(name, args, body))
      }
    })
    .groupBy(_._1)
    .foreach({ (mod, decls) =>
      lower.lower(mod, decls.map({ case (_, importMod, decl) => (importMod, decl) }))
    })
  (lower.decls.toMap, lower.modules.toMap)

private class Lower:
  val modules: mutable.Map[String, Map[String, Symbol]] = mutable.Map()
  val decls: mutable.Map[Symbol, High.Decl] = mutable.Map()

  def declare(mod: String, decl: List[Decl]): Unit =
    decl.foreach(declare(mod, _))

  private def declare(mod: String, decl: Decl): Unit =
    decl match
      case Decl.Def(name, _, _) =>
        val symbol = Symbol.Global(List(mod, name.span.get.text))
        modules.updateWith(mod) {
          case None      => Some(Map(name.span.get.text -> symbol))
          case Some(map) => Some(map + (name.span.get.text -> symbol))
        }
      case Decl.AbsoluteDef(List(Tok.Id(mod), Tok.Id(name)), args, body) =>
        val symbol = Symbol.Global(List(mod, name))
        modules.updateWith(mod) {
          case None      => Some(Map(name -> symbol))
          case Some(map) => Some(map + (name -> symbol))
        }

  def lower(mod: String, decl: List[(String, Decl)]): Unit =
    val globals = modules("Stl")

    decl
      .groupBy { case (_, Decl.Def(name, _, _)) => name.span.get.text }
      .foreach { (name, decl) =>
        decl.head match
          case (_, Decl.Def(_, args, _)) =>
            val arity = args.length
            val symbol = modules(mod)(name)

            assert(decl.forall { case (_, Decl.Def(_, declArgs, _)) => declArgs.length == arity })

            decls(symbol) = lowerDeclGroup(globals, modules.toMap, decl, arity)
      }

  private def lowerDeclGroup(
      globals: Map[String, Symbol],
      modules: Map[String, Map[String, Symbol]],
      decls: List[(String, Decl)],
      arity: Int
  ): High.Decl =
    val argsSym = (1 to arity).map { idx => Symbol.local(s"_arg${idx}_") }.toList

    def fail = letp(PrimOp.TupleNew, argsSym) { args =>
      letl("match error: ") { msg =>
        letp(PrimOp.Concat, List(msg, args))(Tree.Raise(_))
      }
    }
    val e = decls.foldRight(fail) { case ((importMod, Decl.Def(name, pat, body: Expr)), next) =>
      val lower = LowerExpr(globals ++ modules(importMod), modules)
      lower_pat_product(pat.zip(argsSym)) {
        case Some(bindings) =>
          lower.lower_tail(body)(Symbol.Ret)(using bindings)
        case None =>
          next
      }
    }

    High.Decl.Def(argsSym, e)

class LowerExpr(
    val globals: Map[String, Symbol],
    val modules: Map[String, Map[String, Symbol]]
):

  def lower(
      e: Option[Expr]
  )(c: Symbol => Tree)(using sym: Map[Tok.Id | Tok.Op, Symbol]): Tree =
    e
      .map(lower(_)(c))
      .getOrElse(letlNone(c))

  def lower(e: Seq[Expr])(c: List[Symbol] => Tree)(using
      sym: Map[Tok.Id | Tok.Op, Symbol]
  ): Tree =
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
              letl("match error: ") { msg =>
                letp(PrimOp.Concat, List(msg, value))(Tree.Raise(_))
              }
          }
        }

      case Expr.App(Expr.Var(Tok.Id("__intrinsic__")), Expr.Lit(Tok.Lit(op: String)) :: args) =>
        lower(args) { args =>
          letp(PrimOp.valueOf(op), args)(c)
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

      case Expr.Field(Expr.Var(modNm @ Tok.Id(name)), declNm @ Tok.Id(field)) =>
        modules.get(name) match
          case Some(mod) =>
            mod.get(field) match
              case Some(s) => c(s)
              case None =>
                emit(
                  declNm.span.get,
                  s"undefined field ${declNm.span.get.text} in module ${modNm.span.get.text}"
                )
          case None => emit(modNm.span.get, s"undefined module ${modNm.span.get.text}")
        c(modules(name)(field))

      case Expr.Var(name) =>
        sym.get(name).orElse(globals.get(name.span.get.text)) match
          case Some(s) => c(s)
          case None    => emit(name.span.get, s"undefined variable ${name.span.get.text}")

      case Expr.Tuple(_, elems, _) =>
        lower(elems) { args =>
          letp(PrimOp.TupleNew, args)(c)
        }

      case Expr.ListLit(_, elems, _) =>
        lower(elems) { args =>
          letp(PrimOp.ListNew, args)(c)
        }

      case Expr.TupleField(tup, Tok.Lit(idx: Long)) =>
        lower(tup) { tup =>
          letl(idx) { idx =>
            letp(PrimOp.TupleGet, List(tup, idx))(c)
          }
        }

      case Expr.Match(expr, _, cases, _) =>
        val res = Symbol.local
        letc(List(res), c(res)) { c =>
          lower(expr) { expr =>
            def fail = letl("match expr error: ") { msg =>
              letp(PrimOp.Concat, List(msg, expr))(Tree.Raise(_))
            }

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
              letp(PrimOp.TupleNew, argSyms) { args =>
                letl("lambda match error: ") { msg =>
                  letp(PrimOp.Concat, List(msg, args))(Tree.Raise(_))
                }
              }
          },
          c(name)
        )

  private def lower_tail(
      e: Option[Expr]
  )(c: Symbol)(using sym: Map[Tok.Id | Tok.Op, Symbol]): Tree =
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
              letl("match error: ") { msg =>
                letp(PrimOp.Concat, List(msg, value))(Tree.Raise(_))
              }
          }
        }

      case Expr.App(Expr.Var(Tok.Id("__intrinsic__")), Expr.Lit(Tok.Lit(op: String)) :: args) =>
        lower(args) { args =>
          letp(PrimOp.valueOf(op), args)(cf)
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
          letp(PrimOp.TupleNew, args)(cf)
        }

      case Expr.TupleField(tup, Tok.Lit(idx: Long)) =>
        lower(tup) { tup =>
          letl(idx) { idx =>
            letp(PrimOp.TupleGet, List(tup, idx))(cf)
          }
        }

      case Expr.ListLit(_, elems, _) =>
        lower(elems) { args =>
          letp(PrimOp.ListNew, args)(cf)
        }

      case Expr.Match(expr, _, cases, _) =>
        lower(expr) { expr =>
          def fail = letl("match expr error: ") { msg =>
            letp(PrimOp.Concat, List(msg, expr))(Tree.Raise(_))
          }

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
              letp(PrimOp.TupleNew, argSyms) { args =>
                letl("lambda match error: ") { msg =>
                  letp(PrimOp.Concat, List(msg, args))(Tree.Raise(_))
                }
              }
          },
          cf(name)
        )
