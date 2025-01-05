def test_range() = {
  assert(Enum.to_list(range(0, 5)) == [0, 1, 2, 3, 4])
  assert(Enum.to_list(range(2, 5)) == [2, 3, 4])
  assert(Enum.to_list(range(5, 3)) == [])
  assert(Enum.to_list(range(0, 0)) == [])
}

def test_reverse() = {
  assert(Enum.rev([]) == [])
  assert(Enum.rev([0]) == [0])
  assert(Enum.rev([0, 1]) == [1, 0])
  assert(Enum.rev([0, 1, 2]) == [2, 1, 0])
  assert(Enum.rev([0, 1, 2, 3]) == [3, 2, 1, 0])
}

def test_map() = {
  let b = 1
  let addOne = /x/ {
    let f = /x/ x + b
    f(x)
  }

  assert(Enum.map([], addOne) == [])
  assert(Enum.map([0], addOne) == [1])
  assert(Enum.map([0, 1], addOne) == [1, 2])
}
