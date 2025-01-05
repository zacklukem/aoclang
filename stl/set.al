def _new_node() = ('none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none, 'none)

def new() = {
  ('set, _new_node())
}

def put(('set, node), value) = {
  let hash = hash_code(value)
  ('set, _put(node, hash, value, 7))
}

def _put(node, hash, value, 0) = {
  if Enum.contains(node, value) {
    node
  } else {
    value :: node
  }
}

def _put(node, hash, value, depth) = {
  let idx = hash & 31

  let child = match Tuple.get(node, idx) {
    'none => if depth == 1 { [] } else { _new_node() },
    node => node,
  }

  Tuple.put(node, idx, _put(child, hash >> 5, value, depth - 1))
}

def has(('set, node), value) = {
  let hash = hash_code(value)
  _has(node, hash, value, 7)
}

def _has(node: List, hash, value, 0) = {
  Enum.contains(node, value)
}

def _has(node: Tuple, hash, value, depth) = {
  let idx = hash & 31

  match Tuple.get(node, idx) {
    'none => 0 == 1,
    child => _has(child, hash >> 5, value, depth - 1),
  }
}
