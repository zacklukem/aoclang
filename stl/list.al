def head(list) = __intrinsic__("ListHead", list)
def tail(list) = __intrinsic__("ListTail", list)
def is(list) = __intrinsic__("ListIs", list)
def is_empty(list) = __intrinsic__("ListIsEmpty", list)
def to_tuple(list) = __intrinsic__("ListToTuple", list)
def ::(a, b) = __intrinsic__("ListCons", a, b)

def len(arr) = _len(arr, 0)

def _len([], acc) = acc
def _len(h :: tail, acc) = _len(tail, acc + 1)

def range(a, b) = _range(a, b - 1, [])

def _range(a, b, acc) = {
  if b < a {
    acc
  } else {
    _range(a, b - 1, b :: acc)
  }
}

def foreach([], f) = 'none
def foreach(head :: tail, f) = {
  f(head)
  foreach(tail, f)
}

def _rev([], acc) = acc
def _rev(head :: tail, acc) = _rev(tail, head :: acc)

def rev(list) = _rev(list, [])

def map(a, f) = rev(_map(a, f, []))

def _map([], f, acc) = acc
def _map(head :: tail, f, acc) = _map(tail, f, f(head) :: acc)

def reduceWith([], v, f) = v
def reduceWith(head :: tail, v, f) = reduceWith(tail, f(head, v), f)

# TODO: replace with true/false literals
def contains([], v) = 0 == 1
def contains(v :: tail, expected) = {
  if v == expected {
    0 == 0
  } else {
    contains(tail, expected)
  }
}
