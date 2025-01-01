package aoclang

case class Xcept(msg: String) extends Exception(msg)
case class XceptWithStack(msg: String, stack: List[Symbol]) extends Exception(msg)

def xcept(str: String): Nothing = throw Xcept(str)

def intrinsicToString(v: Value): String =
  v match
    case Value.Lit(v)       => v.toString
    case Value.ListVal(l)   => l.map(intrinsicToString).mkString("[", ", ", "]")
    case Value.FnRef(ref)   => s"<$ref>"
    case Value.Tuple(tup)   => tup.map(intrinsicToString).mkString("(", ", ", ")")
    case Value.Cnt(_, _, _) => s"<cnt>"

val INTRINSICS = Map[String, List[Value] => Value](
  "Stl.println" -> {
    case List(a) =>
      println(intrinsicToString(a))
      Value.Lit(Sym.none)
    case _ => xcept("Invalid args for (Stl.println)")
  },
  "Stl.assert" -> {
    case List(Value.Lit(true))  => Value.Lit(Sym.none)
    case List(Value.Lit(false)) => xcept("Assertion failed")
    case _                      => xcept("Invalid types for (Stl.assert)")
  },
  "List.new" -> { args =>
    Value.ListVal(args)
  },
  "List.::" -> {
    case List(head, Value.ListVal(tail)) => Value.ListVal(head :: tail)
    case _                               => xcept("Invalid types for (::)")
  },
  "List.head" -> {
    case List(Value.ListVal(head :: _)) => head
    case List(Value.ListVal(Nil))       => Value.Lit(Sym.none)
    case _                              => xcept("Invalid types for (List.head)")
  },
  "List.tail" -> {
    case List(Value.ListVal(_ :: tail)) => Value.ListVal(tail)
    case List(Value.ListVal(Nil))       => Value.Lit(Sym.none)
    case _                              => xcept("Invalid types for (List.tail)")
  },
  "List.is" -> {
    case List(Value.ListVal(_)) => Value.Lit(true)
    case _                      => Value.Lit(false)
  },
  "List.is_empty" -> {
    case List(Value.ListVal(Nil)) => Value.Lit(true)
    case _                        => Value.Lit(false)
  },
  "Tuple.new" -> { args =>
    Value.Tuple(args.toArray)
  },
  "Tuple.get" -> {
    case List(Value.Tuple(tup), Value.Lit(idx: Long)) => tup(idx.toInt)
    case _                                            => xcept("Invalid types for (Tuple.get)")
  },
  "Tuple.is" -> {
    case List(Value.Tuple(_)) => Value.Lit(true)
    case _                    => Value.Lit(false)
  },
  "Tuple.size" -> {
    case List(Value.Tuple(tup)) => Value.Lit(tup.length.toLong)
    case _                      => xcept("Invalid types for (Tuple.size)")
  },
  "Stl.==" -> { case List(a, b) => Value.Lit(a == b) },
  "Stl.!=" -> { case List(a, b) => Value.Lit(a != b) },
  "Stl.>" -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long))     => Value.Lit(x > y)
    case List(Value.Lit(x: Float), Value.Lit(y: Float))   => Value.Lit(x > y)
    case List(Value.Lit(x: Long), Value.Lit(y: Float))    => Value.Lit(x > y)
    case List(Value.Lit(x: Float), Value.Lit(y: Long))    => Value.Lit(x > y)
    case List(Value.Lit(x: String), Value.Lit(y: String)) => Value.Lit(x > y)
    case _                                                => xcept("Invalid types for (>)")
  },
  "Stl.<" -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long))     => Value.Lit(x < y)
    case List(Value.Lit(x: Float), Value.Lit(y: Float))   => Value.Lit(x < y)
    case List(Value.Lit(x: Long), Value.Lit(y: Float))    => Value.Lit(x < y)
    case List(Value.Lit(x: Float), Value.Lit(y: Long))    => Value.Lit(x < y)
    case List(Value.Lit(x: String), Value.Lit(y: String)) => Value.Lit(x < y)
    case _                                                => xcept("Invalid types for (<)")
  },
  "Stl.>=" -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long))     => Value.Lit(x >= y)
    case List(Value.Lit(x: Float), Value.Lit(y: Float))   => Value.Lit(x >= y)
    case List(Value.Lit(x: Long), Value.Lit(y: Float))    => Value.Lit(x >= y)
    case List(Value.Lit(x: Float), Value.Lit(y: Long))    => Value.Lit(x >= y)
    case List(Value.Lit(x: String), Value.Lit(y: String)) => Value.Lit(x >= y)
    case _                                                => xcept("Invalid types for (>=)")
  },
  "Stl.<=" -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long))     => Value.Lit(x <= y)
    case List(Value.Lit(x: Float), Value.Lit(y: Float))   => Value.Lit(x <= y)
    case List(Value.Lit(x: Long), Value.Lit(y: Float))    => Value.Lit(x <= y)
    case List(Value.Lit(x: Float), Value.Lit(y: Long))    => Value.Lit(x <= y)
    case List(Value.Lit(x: String), Value.Lit(y: String)) => Value.Lit(x <= y)
    case _                                                => xcept("Invalid types for (<=)")
  },
  "Stl.+" -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long))   => Value.Lit(x + y)
    case List(Value.Lit(x: Float), Value.Lit(y: Float)) => Value.Lit(x + y)
    case List(Value.Lit(x: Long), Value.Lit(y: Float))  => Value.Lit(x + y)
    case List(Value.Lit(x: Float), Value.Lit(y: Long))  => Value.Lit(x + y)
    case List(a, b)                                     => xcept(s"Invalid types for (+): $a $b")
  },
  "Stl.-" -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long))   => Value.Lit(x - y)
    case List(Value.Lit(x: Float), Value.Lit(y: Float)) => Value.Lit(x - y)
    case List(Value.Lit(x: Long), Value.Lit(y: Float))  => Value.Lit(x - y)
    case List(Value.Lit(x: Float), Value.Lit(y: Long))  => Value.Lit(x - y)
    case _                                              => xcept("Invalid types for (-)")
  },
  "Stl.*" -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long))   => Value.Lit(x * y)
    case List(Value.Lit(x: Float), Value.Lit(y: Float)) => Value.Lit(x * y)
    case List(Value.Lit(x: Long), Value.Lit(y: Float))  => Value.Lit(x * y)
    case List(Value.Lit(x: Float), Value.Lit(y: Long))  => Value.Lit(x * y)
    case _                                              => xcept("Invalid types for (*)")
  },
  "Stl.**" -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long))   => Value.Lit(Math.pow(x, y).toLong)
    case List(Value.Lit(x: Float), Value.Lit(y: Float)) => Value.Lit(Math.pow(x, y).toLong)
    case List(Value.Lit(x: Long), Value.Lit(y: Float))  => Value.Lit(Math.pow(x, y).toLong)
    case List(Value.Lit(x: Float), Value.Lit(y: Long))  => Value.Lit(Math.pow(x, y).toLong)
    case _                                              => xcept("Invalid types for (**)")
  },
  "Stl.&" -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long)) => Value.Lit(x & y)
    case _                                            => xcept("Invalid types for (&)")
  },
  "Stl.|" -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long)) => Value.Lit(x | y)
    case _                                            => xcept("Invalid types for (|)")
  },
  "Stl.^" -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long)) => Value.Lit(x ^ y)
    case _                                            => xcept("Invalid types for (^)")
  },
  "Stl.>>" -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long)) => Value.Lit(x >> y)
    case _                                            => xcept("Invalid types for (>>)")
  },
  "Stl.<<" -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long)) => Value.Lit(x << y)
    case _                                            => xcept("Invalid types for (<<)")
  },
  "Stl./%" -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long)) => Value.Lit(x / y)
    case _                                            => xcept("Invalid types for (/%)")
  },
  "Stl./" -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long))   => Value.Lit(x.toDouble / y.toDouble)
    case List(Value.Lit(x: Float), Value.Lit(y: Float)) => Value.Lit(x / y)
    case List(Value.Lit(x: Long), Value.Lit(y: Float))  => Value.Lit(x.toDouble / y)
    case List(Value.Lit(x: Float), Value.Lit(y: Long))  => Value.Lit(x / y.toDouble)
    case _                                              => xcept("Invalid types for (/)")
  },
  "Stl.%" -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long)) => Value.Lit(x % y)
    case _                                            => xcept("Invalid types for (%)")
  },
  "Stl.++" -> {
    case List(a, b) => Value.Lit(s"${intrinsicToString(a)}${intrinsicToString(b)}")
    case _          => xcept(s"Invalid types for (++)")
  }
)
