def test_len_of_empty_list() = {
  assert(List.len([]) == 0)
}

def test_len_of_non_empty_list() = {
  assert(List.len([0]) == 1)
  assert(List.len([0, 1, 2]) == 3)
}

def test_range() = {
  assert(range(0, 5) == [0, 1, 2, 3, 4])
  assert(range(2, 5) == [2, 3, 4])
  assert(range(5, 3) == [])
  assert(range(0, 0) == [])
}

def test_reverse() = {
  assert(rev([]) == [])
  assert(rev([0]) == [0])
  assert(rev([0, 1]) == [1, 0])
  assert(rev([0, 1, 2]) == [2, 1, 0])
  assert(rev([0, 1, 2, 3]) == [3, 2, 1, 0])
}

def test_map() = {
  assert(map([], /x/ x + 1) == [])
  assert(map([0], /x/ x + 1) == [1])
  assert(map([0, 1], /x/ x + 1) == [1, 2])
}
