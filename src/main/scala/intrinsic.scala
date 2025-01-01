package aoclang

def xcept(str: String): Nothing = throw new Exception(str)

def intrinsicToString(v: Value): String =
  v match
    case Value.Lit(v)     => v.toString
    case Value.FnRef(ref) => s"<$ref>"
    case Value.Tuple(tup) => tup.map(intrinsicToString).mkString("(", ", ", ")")

val INTRINSICS = Map[String, List[Value] => Value](
  "Stl.println" -> { case List(a) =>
    println(intrinsicToString(a))
    Value.Lit(Sym.none)
  },
  "List.new" -> { args =>
    args.foldRight(Value.Lit(Sym.nil)) { case (a, b) =>
      Value.Tuple(Array(a, b))
    }
  },
  "Tuple.new" -> { args =>
    Value.Tuple(args.toArray)
  },
  "Tuple.get" -> { case List(Value.Tuple(tup), Value.Lit(idx: Long)) =>
    tup(idx.toInt)
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
    case List(a, b) => xcept(s"Invalid types for (++): $a $b")
  }
)
