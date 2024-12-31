package aoclang

import scala.annotation.targetName

class Source(val text: String):
  def span(start: Int, end: Int): Span = Span(this, start, end)

case class Span(source: Source, start: Int, end: Int):
  def text: String = source.text.substring(start, end)

  def lineColumn: (Int, Int) =
    val line = source.text.slice(0, start).count(_ == '\n') + 1
    val column = source.text.slice(0, start).lastIndexOf('\n') match
      case -1          => start + 1
      case lastNewline => start - lastNewline
    (line, column)

  def printLineColumn(message: String) =
    val (line, column) = lineColumn
    println(
      s"$line:$column: $message\n${source.text.split('\n')(line - 1)}\n${" " * (column - 1)}^"
    )

  @targetName("concat")
  def ++(other: Span): Span =
    Span(source, start, other.end)

object NullSpan extends Span(null, 0, 0)
