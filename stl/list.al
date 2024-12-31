
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

def rev_tail(list, acc) = {
  if list == 'nil {
    acc
  } else {
    let (head, tail) = list
    rev_tail(tail, head :: acc)
  }
}

def rev(list) = {
  rev_tail(list, 'nil)
}

def map(a, f) = {
  if a == 'nil {
    'nil
  } else {
    let (head, tail) = a
    f(head) :: map(tail, f)
  }
}
