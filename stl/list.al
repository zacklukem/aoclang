def new(args) = intrinsic
def head(list) = intrinsic
def tail(list) = intrinsic
def is(list) = intrinsic
def is_empty(list) = intrinsic
def to_tuple(list) = intrinsic
def ::(a, b) = intrinsic

def len([]) = 0
def len(h :: tail) = {
  1 + len(tail)
}

def range(a, b) = {
  if a >= b {
    []
  } else {
    a :: range(a + 1, b)
  }
}

def foreach(a, f) = {
  if a != [] {
    let head :: tail = a
    f(head)
    foreach(tail, f)
  }
}

def rev_tail([], acc) = acc
def rev_tail(head :: tail, acc) = rev_tail(tail, head :: acc)

def rev(list) = rev_tail(list, [])

def map(a, f) = {
  match a {
    [] => [],
    head :: tail => f(head) :: map(tail, f),
  }
}

# TODO: replace with true/false literals
def contains([], v) = 0 == 1
def contains(v :: tail, expected) = {
  if v == expected {
    0 == 0
  } else {
    contains(tail, expected)
  }
}
