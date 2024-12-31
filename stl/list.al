
def ::(a, b) = {
  (a, b)
}

def len(a) = {
  if a == 'nil {
    0
  } else {
    1 + len(a.1)
  }
}

def range(a, b) = {
  if a >= b {
    'nil
  } else {
    a :: range(a + 1, b)
  }
}

def foreach(a, f) = {
  if a != 'nil {
    let (head, tail) = a
    f(head)
    foreach(tail, f)
  }
}

def rev_tail('nil, acc) = acc
def rev_tail((head, tail), acc) = rev_tail(tail, head :: acc)

def rev(list) = rev_tail(list, 'nil)

def map(a, f) = {
  match a {
    'nil => 'nil,
    (head, tail) => f(head) :: map(tail, f),
  }
}
