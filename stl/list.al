
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
