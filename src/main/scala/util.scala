package aoclang

case class Sym(name: String):
  override def toString: String = s"'$name"

extension [A](x: A) def |>[B](f: A => B): B = f(x)
