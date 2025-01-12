package aoclang

import java.nio.file.{Files, Path}

case class Xcept(msg: String) extends Exception(msg)
case class XceptWithStack(msg: String, stack: List[Low.Name]) extends Exception(msg)

def xcept(str: String): Nothing = throw Xcept(str)

def intrinsicToString(v: Value): String =
  v match
    case Value.Lit(v)         => v.toString
    case Value.ListVal(l)     => l.map(intrinsicToString).mkString("[", ", ", "]")
    case Value.Closure(fn, _) => s"<$fn>"
    case Value.Ref(v)         => intrinsicToString(v)
    case Value.Tuple(tup)     => tup.map(intrinsicToString).mkString("(", ", ", ")")
    case Value.Cnt(_, _)      => s"<cnt>"

val INTRINSICS = Map[PrimOp, List[Value] => Value](
  PrimOp.Ref -> {
    case List(v) => Value.Ref(v)
    case _       => xcept("Invalid types for (Stl.ref)")
  },
  PrimOp.Store -> {
    case List(v: Value.Ref, x) =>
      v.v = x
      Value.Lit(Sym.none)
    case _ => xcept("Invalid types for (Stl.store)")
  },
  PrimOp.Load -> {
    case List(v: Value.Ref) => v.v
    case _                  => xcept("Invalid types for (Stl.load)")
  },
  PrimOp.PrintLine -> {
    case List(a) =>
      println(intrinsicToString(a))
      Value.Lit(Sym.none)
    case _ => xcept("Invalid args for (Stl.println)")
  },
  PrimOp.Assert -> {
    case List(Value.Lit(true))  => Value.Lit(Sym.none)
    case List(Value.Lit(false)) => xcept("Assertion failed")
    case _                      => xcept("Invalid types for (Stl.assert)")
  },
  PrimOp.AssertEq -> {
    case List(a, b) if a == b => Value.Lit(Sym.none)
    case List(a, b) =>
      xcept(s"Assertion failed: ${intrinsicToString(a)} != ${intrinsicToString(b)}")
    case _ => xcept("Invalid types for (Stl.assert)")
  },
  PrimOp.HashCode -> {
    case List(x) => Value.Lit(x.hashCode.toLong)
    case _       => xcept("Invalid types for (Stl.hash_code)")
  },
  PrimOp.FileReadString -> {
    case List(Value.Lit(path: String)) =>
      try
        val s = Files.readString(Path.of(path))
        Value.Lit(s)
      catch case e => xcept(s"Failed to read file")
    case _ => xcept("Invalid types for (File.read_string)")
  },
  PrimOp.StringChars -> {
    case List(Value.Lit(s: String)) => Value.ListVal(s.toList.map(_.toLong |> Value.Lit.apply))
    case _                          => xcept("Invalid types for (String.chars)")
  },
  PrimOp.StringSize -> {
    case List(Value.Lit(s: String)) => Value.Lit(s.length.toLong)
    case _                          => xcept("Invalid types for (String.size)")
  },
  PrimOp.StringFromInt -> {
    case List(Value.Lit(i: Long)) => Value.Lit(i.toString)
    case _                        => xcept("Invalid types for (String.from_int)")
  },
  PrimOp.StringFromChars -> {
    case List(Value.ListVal(chars)) =>
      chars.map {
        case Value.Lit(c: Long) => c.toChar
        case _                  => xcept("Invalid types for (String.from_chars)")
      }.mkString
        |> Value.Lit.apply
    case _ => xcept("Invalid types for (String.from_chars)")
  },
  PrimOp.StringSplit -> {
    case List(Value.Lit(s: String), Value.Lit(delim: String)) =>
      s.split(delim).map(Value.Lit.apply).toList |> Value.ListVal.apply
    case _ => xcept("Invalid types for (String.split)")
  },
  PrimOp.IntIs -> {
    case List(Value.Lit(_: Long)) => Value.Lit(true)
    case _                        => Value.Lit(false)
  },
  PrimOp.IntFromString -> {
    case List(Value.Lit(s: String)) =>
      try Value.Lit(s.toLong)
      catch case e => Value.Lit(Sym.none)
    case _ => xcept("Invalid types for (Int.from_string)")
  },
  PrimOp.ListNew -> { args =>
    Value.ListVal(args)
  },
  PrimOp.ListCons -> {
    case List(head, Value.ListVal(tail)) => Value.ListVal(head :: tail)
    case _                               => xcept("Invalid types for (::)")
  },
  PrimOp.ListHead -> {
    case List(Value.ListVal(head :: _)) => Value.Tuple(Array(Value.Lit(Sym.some), head))
    case List(Value.ListVal(Nil))       => Value.Lit(Sym.none)
    case _                              => xcept("Invalid types for (List.head)")
  },
  PrimOp.ListTail -> {
    case List(Value.ListVal(_ :: tail)) => Value.ListVal(tail)
    case List(Value.ListVal(Nil))       => Value.Lit(Sym.none)
    case _                              => xcept("Invalid types for (List.tail)")
  },
  PrimOp.ListIs -> {
    case List(Value.ListVal(_)) => Value.Lit(true)
    case _                      => Value.Lit(false)
  },
  PrimOp.ListIsEmpty -> {
    case List(Value.ListVal(Nil)) => Value.Lit(true)
    case _                        => Value.Lit(false)
  },
  PrimOp.ListToTuple -> {
    case List(Value.ListVal(l)) => Value.Tuple(l.toArray)
    case _                      => xcept("Invalid types for (List.to_tuple)")
  },
  PrimOp.TupleNew -> { args =>
    Value.Tuple(args.toArray)
  },
  PrimOp.TupleGet -> {
    case List(Value.Tuple(tup), Value.Lit(idx: Long)) => tup(idx.toInt)
    case x                                            => xcept(s"Invalid types for (Tuple.get) $x")
  },
  PrimOp.TupleIs -> {
    case List(Value.Tuple(_)) => Value.Lit(true)
    case _                    => Value.Lit(false)
  },
  PrimOp.TupleSize -> {
    case List(Value.Tuple(tup)) => Value.Lit(tup.length.toLong)
    case _                      => xcept("Invalid types for (Tuple.size)")
  },
  PrimOp.TuplePut -> {
    case List(Value.Tuple(tup), Value.Lit(idx: Long), v) =>
      val idxInt = idx.toInt
      val newTup = tup.updated(idxInt, v)
      Value.Tuple(newTup)
    case _ => xcept("Invalid types for (Tuple.put)")
  },
  PrimOp.Not -> {
    case List(Value.Lit(x: Boolean)) => Value.Lit(!x)
    case _                           => xcept("Invalid types for (!)")
  },
  PrimOp.Eq -> { case List(a, b) => Value.Lit(a == b) },
  PrimOp.Neq -> { case List(a, b) => Value.Lit(a != b) },
  PrimOp.Gt -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long))     => Value.Lit(x > y)
    case List(Value.Lit(x: Double), Value.Lit(y: Double)) => Value.Lit(x > y)
    case List(Value.Lit(x: Long), Value.Lit(y: Double))   => Value.Lit(x > y)
    case List(Value.Lit(x: Double), Value.Lit(y: Long))   => Value.Lit(x > y)
    case List(Value.Lit(x: String), Value.Lit(y: String)) => Value.Lit(x > y)
    case _                                                => xcept("Invalid types for (>)")
  },
  PrimOp.Lt -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long))     => Value.Lit(x < y)
    case List(Value.Lit(x: Double), Value.Lit(y: Double)) => Value.Lit(x < y)
    case List(Value.Lit(x: Long), Value.Lit(y: Double))   => Value.Lit(x < y)
    case List(Value.Lit(x: Double), Value.Lit(y: Long))   => Value.Lit(x < y)
    case List(Value.Lit(x: String), Value.Lit(y: String)) => Value.Lit(x < y)
    case _                                                => xcept("Invalid types for (<)")
  },
  PrimOp.Ge -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long))     => Value.Lit(x >= y)
    case List(Value.Lit(x: Double), Value.Lit(y: Double)) => Value.Lit(x >= y)
    case List(Value.Lit(x: Long), Value.Lit(y: Double))   => Value.Lit(x >= y)
    case List(Value.Lit(x: Double), Value.Lit(y: Long))   => Value.Lit(x >= y)
    case List(Value.Lit(x: String), Value.Lit(y: String)) => Value.Lit(x >= y)
    case _                                                => xcept("Invalid types for (>=)")
  },
  PrimOp.Le -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long))     => Value.Lit(x <= y)
    case List(Value.Lit(x: Double), Value.Lit(y: Double)) => Value.Lit(x <= y)
    case List(Value.Lit(x: Long), Value.Lit(y: Double))   => Value.Lit(x <= y)
    case List(Value.Lit(x: Double), Value.Lit(y: Long))   => Value.Lit(x <= y)
    case List(Value.Lit(x: String), Value.Lit(y: String)) => Value.Lit(x <= y)
    case _                                                => xcept("Invalid types for (<=)")
  },
  PrimOp.Add -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long))     => Value.Lit(x + y)
    case List(Value.Lit(x: Double), Value.Lit(y: Double)) => Value.Lit(x + y)
    case List(Value.Lit(x: Long), Value.Lit(y: Double))   => Value.Lit(x + y)
    case List(Value.Lit(x: Double), Value.Lit(y: Long))   => Value.Lit(x + y)
    case List(a, b)                                       => xcept(s"Invalid types for (+): $a $b")
  },
  PrimOp.Sub -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long))     => Value.Lit(x - y)
    case List(Value.Lit(x: Double), Value.Lit(y: Double)) => Value.Lit(x - y)
    case List(Value.Lit(x: Long), Value.Lit(y: Double))   => Value.Lit(x - y)
    case List(Value.Lit(x: Double), Value.Lit(y: Long))   => Value.Lit(x - y)
    case _                                                => xcept("Invalid types for (-)")
  },
  PrimOp.Mul -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long))     => Value.Lit(x * y)
    case List(Value.Lit(x: Double), Value.Lit(y: Double)) => Value.Lit(x * y)
    case List(Value.Lit(x: Long), Value.Lit(y: Double))   => Value.Lit(x * y)
    case List(Value.Lit(x: Double), Value.Lit(y: Long))   => Value.Lit(x * y)
    case _                                                => xcept("Invalid types for (*)")
  },
  PrimOp.Pow -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long))     => Value.Lit(Math.pow(x, y).toLong)
    case List(Value.Lit(x: Double), Value.Lit(y: Double)) => Value.Lit(Math.pow(x, y))
    case List(Value.Lit(x: Long), Value.Lit(y: Double))   => Value.Lit(Math.pow(x, y))
    case List(Value.Lit(x: Double), Value.Lit(y: Long))   => Value.Lit(Math.pow(x, y))
    case _                                                => xcept("Invalid types for (**)")
  },
  PrimOp.BAnd -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long)) => Value.Lit(x & y)
    case _                                            => xcept("Invalid types for (&)")
  },
  PrimOp.BOr -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long)) => Value.Lit(x | y)
    case _                                            => xcept("Invalid types for (|)")
  },
  PrimOp.Xor -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long)) => Value.Lit(x ^ y)
    case _                                            => xcept("Invalid types for (^)")
  },
  PrimOp.Shr -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long)) => Value.Lit(x >> y)
    case _                                            => xcept("Invalid types for (>>)")
  },
  PrimOp.Shl -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long)) => Value.Lit(x << y)
    case _                                            => xcept("Invalid types for (<<)")
  },
  PrimOp.DivInt -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long)) => Value.Lit(x / y)
    case _                                            => xcept("Invalid types for (/%)")
  },
  PrimOp.DivFloat -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long))     => Value.Lit(x.toDouble / y.toDouble)
    case List(Value.Lit(x: Double), Value.Lit(y: Double)) => Value.Lit(x / y)
    case List(Value.Lit(x: Long), Value.Lit(y: Double))   => Value.Lit(x.toDouble / y)
    case List(Value.Lit(x: Double), Value.Lit(y: Long))   => Value.Lit(x / y.toDouble)
    case _                                                => xcept("Invalid types for (/)")
  },
  PrimOp.Mod -> {
    case List(Value.Lit(x: Long), Value.Lit(y: Long)) => Value.Lit(x % y)
    case _                                            => xcept("Invalid types for (%)")
  },
  PrimOp.Concat -> {
    case List(a, b) => Value.Lit(s"${intrinsicToString(a)}${intrinsicToString(b)}")
    case _          => xcept(s"Invalid types for (++)")
  }
)
