
def test_ref() = {
  let x = ref(5)
  x <- 6
  assert_eq(x, x)
  assert_eq(@x, 6)
  assert(x != ref(6))
}

# TODO: tail recursion optimization
# def tail_rec(0) = true
# def tail_rec(n) = tail_rec(n - 1)
#
# def test_tail_rec() = {
#   assert(tail_rec(50000))
# }
