package aoclang

case class Sym(name: String):
  override def toString: String = s"'$name"

object Sym:
  val none = Sym("none")
  val nil = Sym("nil")

extension [A](x: A) def |>[B](f: A => B): B = f(x)
