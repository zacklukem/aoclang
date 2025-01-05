package aoclang

import scala.annotation.targetName

case class Sym(name: String):
  override def toString: String = s"'$name"

object Sym:
  val none: Sym = Sym("none")
  val some: Sym = Sym("some")

extension [A](x: A)
  @targetName("pipe")
  def |>[B](f: A => B): B = f(x)
