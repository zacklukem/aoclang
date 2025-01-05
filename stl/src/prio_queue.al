
def _root((x, r, c)) = x
def _rank((x, r, c)) = r
def _leq((x1p, _), (x2p, _)) = x1p <= x2p
def _link((x1, r1, c1), (x2, r2, c2)) =
  if _leq(x1, x2) {
    (x1, r1 + 1, (x2, r2, c2) :: c1)
  } else {
    (x2, r2 + 1, (x1, r1, c1) :: c2)
  }

def _ins(t, []) = [t]
def _ins(t, t' :: ts) =
  if _rank(t) < _rank(t') {
   t :: t' :: ts
  } else {
    _ins(_link(t, t'), ts)
  }

def _mk(ts) = ('prio_queue, ts)

def empty() = ('prio_queue, [])
def isEmpty(('prio_queue, ts)) = ts == []

def insert(('prio_queue, ts), prio, val) = {
  let x = (prio, val)
  _mk(_ins((x, 0, []), ts))
}

def meld(('prio_queue, ts1), ('prio_queue, ts2)) = _mk(_meld(ts1, ts2))
def _meld([], ts) = ts
def _meld(ts, []) = ts
def _meld(t1 :: ts1, t2 :: ts2) =
  if _rank(t1) < _rank(t2) {
    t1 :: _meld(ts1, t2 :: ts2)
  } else { # TODO: else if
    if _rank(t2) < _rank(t1) {
      t2 :: _meld(t1 :: ts1, ts2)
    } else {
      _ins(_link(t1, t2), _meld(ts1, ts2))
    }
  }

# returns (prio, elem) or 'none
def findMin(('prio_queue, ts)) = _findMin(ts)
def _findMin([]) = 'none
def _findMin([t]) = _root(t)
def _findMin(t :: ts) = {
  let x = _findMin(ts)
  if _leq(_root(t), x) {
    _root(t)
  } else {
    x
  }
}

def deleteMin(('prio_queue, ts)) = _mk(_deleteMin(ts))
def _deleteMin([]) = 'none
def _deleteMin(ts) = {
  let ((x, r, c), ts) = _getMin(ts)
  _meld(Enum.rev(c), ts)
}

def _getMin([t]) = (t, [])
def _getMin(t :: ts) = {
  let (t', ts') = _getMin(ts)
  if _leq(_root(t), _root(t')) {
    (t, ts)
  } else {
    (t', t :: ts')
  }
}

def _print(('prio_queue, ts)) = {
  println("root")
  __print(ts, "  ")
}
def __print(ts, indent) = {
  ts |> Enum.foreach(/t/ {
    let (x, r, c) = t
    println(indent ++ x)
    __print(c, indent ++ "  ")
  })
}
