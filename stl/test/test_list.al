def test_len_of_empty_list() = {
  assert_eq(List.len([]), 0)
}

def test_len_of_non_empty_list() = {
  assert_eq(List.len([0]), 1)
  assert_eq(List.len([0, 1, 2]), 3)
}

def test_cons() = {
  assert_eq((1 :: 1 + 1 + 3 * 0 :: 3 :: []), [1, 2, 3])
}
