
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

def new() = ('prio_queue, [])
def is_empty(('prio_queue, ts)) = ts == []
def is(('prio_queue, ts)) = true
def is(_) = false

def enqueue(('prio_queue, ts), prio, val) = {
  let x = (prio, val)
  _mk(_ins((x, 0, []), ts))
}

def dequeue(('prio_queue, ts)) = {
  let min = _min(ts)
  let q = _mk(_delete_min(ts))
  (q, min)
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
def min(('prio_queue, ts)) = _min(ts)
def _min([]) = 'none
def _min([t]) = _root(t)
def _min(t :: ts) = {
  let x = _min(ts)
  if _leq(_root(t), x) {
    _root(t)
  } else {
    x
  }
}

def delete_min(('prio_queue, ts)) = _mk(_delete_min(ts))
def _delete_min([]) = PrioQueue.new()
def _delete_min(ts) = {
  let ((x, r, c), ts) = _get_min(ts)
  _meld(Enum.rev(c), ts)
}

def _get_min([t]) = (t, [])
def _get_min(t :: ts) = {
  let (t', ts') = _get_min(ts)
  if _leq(_root(t), _root(t')) {
    (t, ts)
  } else {
    (t', t :: ts')
  }
}

def Enumerable.from(q: PrioQueue) = q
def Enumerable.next(q: PrioQueue) = match PrioQueue.dequeue(q) {
    (q, 'none) => (q, 'none),
    (q, v) => (q, ('some, v)),
  }
