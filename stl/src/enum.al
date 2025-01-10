def sort(enum) = {
  enum
    |> reduceWith(PrioQueue.new(), /v, q/ {
      q |> PrioQueue.enqueue(v, v)
    })
    |> map(/x/ x.0)
}

def take(enum, n) = rev(_take(Enumerable.from(enum), n, []))
def _take(enum, 0, acc) = acc
def _take(enum, n, acc) = match Enumerable.next(enum) {
  (_, 'none) => acc,
  (enum, ('some, v)) => _take(enum, n - 1, v :: acc),
}

def map(enum, f) = rev(_map(Enumerable.from(enum), f, []))
def _map(enum, f, acc) = match Enumerable.next(enum) {
    (_, 'none) => acc,
    (enum, ('some, v)) => _map(enum, f, f(v) :: acc),
  }

def reduce(enum, f) = _reduce(Enumerable.from(enum), f)
def _reduce(enum, f) = match Enumerable.next(enum) {
    (enum, ('some, head)) => _reduceWith(enum, head, f)
  }

def sum(enum) = reduceWith(enum, 0, /a, b/ a + b)
def max(enum) = reduce(enum, Math.max)
def min(enum) = reduce(enum, Math.min)

def reduceWith(enum, acc, f) = _reduceWith(Enumerable.from(enum), acc, f)
def _reduceWith(enum, acc, f) = match Enumerable.next(enum) {
    (_, 'none) => acc,
    (enum, ('some, head)) => _reduceWith(enum, f(head, acc), f)
  }

def rev(enum) = _rev(Enumerable.from(enum), [])
def _rev(enum, acc) = match Enumerable.next(enum) {
    (_, 'none) => acc,
    (enum, ('some, v)) => _rev(enum, v :: acc),
  }

def foreach(enum, f) = _foreach(Enumerable.from(enum), f)
def _foreach(enum, f) = match Enumerable.next(enum) {
    (_, 'none) => 'none,
    (enum, ('some, v)) => {
      f(v)
      _foreach(enum, f)
    },
  }

def contains(enum, v) = _contains(Enumerable.from(enum), v)
def _contains(enum, v) = match Enumerable.next(enum) {
    (_, 'none) => false,
    (enum, ('some, v')) => if v == v' { true } else { _contains(enum, v) }
  }

def to_list(enum) = rev(rev(enum))
