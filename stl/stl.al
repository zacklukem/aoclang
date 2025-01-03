def println(a) = __intrinsic__("PrintLine", a)

def assert(a) = __intrinsic__("Assert", a)

def hash_code(a) = __intrinsic__("HashCode", a)

def ==(a, b) = __intrinsic__("Eq", a, b)

def !=(a, b) = __intrinsic__("Neq", a, b)

def <(a, b) = __intrinsic__("Lt", a, b)

def >(a, b) = __intrinsic__("Gt", a, b)

def <=(a, b) = __intrinsic__("Le", a, b)

def >=(a, b) = __intrinsic__("Ge", a, b)

def +(a, b) = __intrinsic__("Add", a, b)

def -(a, b) = __intrinsic__("Sub", a, b)

def *(a, b) = __intrinsic__("Mul", a, b)

def /%(a, b) = __intrinsic__("DivInt", a, b)

def /(a, b) = __intrinsic__("DivFloat", a, b)

def %(a, b) = __intrinsic__("Mod", a, b)

def **(a, b) = __intrinsic__("Pow", a, b)

def &(a, b) = __intrinsic__("BAnd", a, b)

def |(a, b) = __intrinsic__("BOr", a, b)

def ^(a, b) = __intrinsic__("Xor", a, b)

def <<(a, b) = __intrinsic__("Shl", a, b)

def >>(a, b) = __intrinsic__("Shr", a, b)

def ++(a, b) = __intrinsic__("Concat", a, b)

def !(a) = __intrinsic__("Not", a)

def |>(a, f) = f(a)
