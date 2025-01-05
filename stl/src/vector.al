def _new_node() = ('none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none)

def new() = {
  ('vector, _new_node(), 0)
}

def size(('vector, _, size)) = size

def put(('vector, node, size), idx, value) = {
  ('vector, _put(node, idx, value, 7), Math.max(size, idx + 1))
}

def _put(node, idx, value, 0) = {
  ('some, value)
}

def _put(node, idx, value, depth) = {
  let tidx = idx & 31

  let child = match Tuple.get(node, tidx) {
    'none => if depth == 1 { [] } else { _new_node() },
    node => node,
  }

  Tuple.put(node, tidx, _put(child, idx >> 5, value, depth - 1))
}

def get(('vector, node, size), idx) = {
  _get(node, idx, 7)
}

def _get(node, idx, 0) = {
  node
}

def _get(node, idx, depth) = {
  let tidx = idx & 31

  match Tuple.get(node, tidx) {
    'none => 0 == 1,
    child => _get(child, idx >> 5, depth - 1),
  }
}
