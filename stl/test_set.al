def test_set_new() = {
  let ('set, root) = Set.new()
  assert(Tuple.size(root) == 32)
}

def test_set_put() = {
  let set = Set.new()

  let set = Set.put(set, 5)
  let set = Set.put(set, 5)
  let set = Set.put(set, 7)

  assert(Set.has(set, 5))
  assert(!Set.has(set, 6))
}