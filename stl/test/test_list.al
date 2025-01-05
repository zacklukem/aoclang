def test_len_of_empty_list() = {
  assert(List.len([]) == 0)
}

def test_len_of_non_empty_list() = {
  assert(List.len([0]) == 1)
  assert(List.len([0, 1, 2]) == 3)
}

def test_cons() = {
  assert((1 :: 1 + 1 + 3 * 0 :: 3 :: []) == [1, 2, 3])
}
