package aoclang

def xcept(str: String): Nothing = throw new Exception(str)

def intrinsicToString(v: Value) =
  v match
    case Value.Lit(v)     => v.toString
    case Value.FnRef(ref) => s"<${ref}>"

val INTRINSICS = Map[String, List[Value] => Value](
  "println" -> { case List(a) =>
    println(intrinsicToString(a))
    Value.Lit(Sym.none)
  },
  "==" -> { case List(a, b) => Value.Lit(a == b) },
  "!=" -> { case List(a, b) => Value.Lit(a != b) },
  ">" -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long))     => Value.Lit(x > y)
    case List(Value.Lit(x: Float), Value.Lit(y: Float))   => Value.Lit(x > y)
    case List(Value.Lit(x: Long), Value.Lit(y: Float))    => Value.Lit(x > y)
    case List(Value.Lit(x: Float), Value.Lit(y: Long))    => Value.Lit(x > y)
    case List(Value.Lit(x: String), Value.Lit(y: String)) => Value.Lit(x > y)
    case _                                                => xcept("Invalid types for (>)")
  },
  "<" -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long))     => Value.Lit(x < y)
    case List(Value.Lit(x: Float), Value.Lit(y: Float))   => Value.Lit(x < y)
    case List(Value.Lit(x: Long), Value.Lit(y: Float))    => Value.Lit(x < y)
    case List(Value.Lit(x: Float), Value.Lit(y: Long))    => Value.Lit(x < y)
    case List(Value.Lit(x: String), Value.Lit(y: String)) => Value.Lit(x < y)
    case _                                                => xcept("Invalid types for (<)")
  },
  ">=" -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long))     => Value.Lit(x >= y)
    case List(Value.Lit(x: Float), Value.Lit(y: Float))   => Value.Lit(x >= y)
    case List(Value.Lit(x: Long), Value.Lit(y: Float))    => Value.Lit(x >= y)
    case List(Value.Lit(x: Float), Value.Lit(y: Long))    => Value.Lit(x >= y)
    case List(Value.Lit(x: String), Value.Lit(y: String)) => Value.Lit(x >= y)
    case _                                                => xcept("Invalid types for (>=)")
  },
  "<=" -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long))     => Value.Lit(x <= y)
    case List(Value.Lit(x: Float), Value.Lit(y: Float))   => Value.Lit(x <= y)
    case List(Value.Lit(x: Long), Value.Lit(y: Float))    => Value.Lit(x <= y)
    case List(Value.Lit(x: Float), Value.Lit(y: Long))    => Value.Lit(x <= y)
    case List(Value.Lit(x: String), Value.Lit(y: String)) => Value.Lit(x <= y)
    case _                                                => xcept("Invalid types for (<=)")
  },
  "+" -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long))   => Value.Lit(x + y)
    case List(Value.Lit(x: Float), Value.Lit(y: Float)) => Value.Lit(x + y)
    case List(Value.Lit(x: Long), Value.Lit(y: Float))  => Value.Lit(x + y)
    case List(Value.Lit(x: Float), Value.Lit(y: Long))  => Value.Lit(x + y)
    case List(a, b)                                     => xcept(s"Invalid types for (+): $a $b")
  },
  "-" -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long))   => Value.Lit(x - y)
    case List(Value.Lit(x: Float), Value.Lit(y: Float)) => Value.Lit(x - y)
    case List(Value.Lit(x: Long), Value.Lit(y: Float))  => Value.Lit(x - y)
    case List(Value.Lit(x: Float), Value.Lit(y: Long))  => Value.Lit(x - y)
    case _                                              => xcept("Invalid types for (-)")
  },
  "*" -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long))   => Value.Lit(x * y)
    case List(Value.Lit(x: Float), Value.Lit(y: Float)) => Value.Lit(x * y)
    case List(Value.Lit(x: Long), Value.Lit(y: Float))  => Value.Lit(x * y)
    case List(Value.Lit(x: Float), Value.Lit(y: Long))  => Value.Lit(x * y)
    case _                                              => xcept("Invalid types for (*)")
  },
  "**" -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long))   => Value.Lit(Math.pow(x, y).toLong)
    case List(Value.Lit(x: Float), Value.Lit(y: Float)) => Value.Lit(Math.pow(x, y).toLong)
    case List(Value.Lit(x: Long), Value.Lit(y: Float))  => Value.Lit(Math.pow(x, y).toLong)
    case List(Value.Lit(x: Float), Value.Lit(y: Long))  => Value.Lit(Math.pow(x, y).toLong)
    case _                                              => xcept("Invalid types for (**)")
  },
  "&" -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long)) => Value.Lit(x & y)
    case _                                            => xcept("Invalid types for (&)")
  },
  "|" -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long)) => Value.Lit(x | y)
    case _                                            => xcept("Invalid types for (|)")
  },
  "^" -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long)) => Value.Lit(x ^ y)
    case _                                            => xcept("Invalid types for (^)")
  },
  ">>" -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long)) => Value.Lit(x >> y)
    case _                                            => xcept("Invalid types for (>>)")
  },
  "<<" -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long)) => Value.Lit(x << y)
    case _                                            => xcept("Invalid types for (<<)")
  },
  "/%" -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long)) => Value.Lit(x / y)
    case _                                            => xcept("Invalid types for (/%)")
  },
  "/" -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long))   => Value.Lit(x.toDouble / y.toDouble)
    case List(Value.Lit(x: Float), Value.Lit(y: Float)) => Value.Lit(x / y)
    case List(Value.Lit(x: Long), Value.Lit(y: Float))  => Value.Lit(x.toDouble / y)
    case List(Value.Lit(x: Float), Value.Lit(y: Long))  => Value.Lit(x / y.toDouble)
    case _                                              => xcept("Invalid types for (/)")
  },
  "%" -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long)) => Value.Lit(x % y)
    case _                                            => xcept("Invalid types for (%)")
  },
  "++" -> {
    case List(Value.Lit(x), Value.Lit(y)) => Value.Lit(s"$x$y")
    case List(a, b)                       => xcept(s"Invalid types for (+): $a $b")
  }
)
