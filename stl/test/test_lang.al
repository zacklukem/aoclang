
def test_ref() = {
  let x = ref(5)
  x <- 6
  assert_eq(x, x)
  assert_eq(@x, 6)
  assert(x != ref(6))
}
