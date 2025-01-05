
def map(enum, f) = rev(_map(Enumerable.from(enum), f, []))
def _map(enum, f, acc) = match Enumerable.next(enum) {
    (_, 'none) => acc,
    (enum, ('some, v)) => _map(enum, f, f(v) :: acc),
  }

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
    # TODO: boolean literals
    (_, 'none) => 0 == 1,
    (enum, ('some, v')) => if v == v' { 0 == 0 } else { _contains(enum, v) }
  }

def to_list(enum) = rev(rev(enum))
