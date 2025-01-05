
def test_ref() = {
  let x = ref(5)
  x <- 6
  assert(x == x)
  assert(@x == 6)
  assert(x != ref(6))
}
