
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
    f(a.0)
    foreach(a.1, f)
  }
}

def rev_tail(list, acc) = {
  if list == 'nil {
    acc
  } else {
    rev_tail(list.1, list.0 :: acc)
  }
}

def rev(list) = {
  rev_tail(list, 'nil)
}

def map(a, f) = {
  if a == 'nil {
    'nil
  } else {
    f(a.0) :: map(a.1, f)
  }
}
