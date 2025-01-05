def test_len_of_empty_list() = {
  assert(List.len([]) == 0)
}

def test_len_of_non_empty_list() = {
  assert(List.len([0]) == 1)
  assert(List.len([0, 1, 2]) == 3)
}
