def test_vector() = {
  let vec = Vector.new()
  let vec = Vector.put(vec, 0, 'hello0)
  let vec = Vector.put(vec, 5, 'hello5)
  let vec = Vector.put(vec, 1000, 'hello1000)
  let vec = Vector.put(vec, 8000, 'hello8000)

  assert_eq(Vector.get(vec, 0), ('some, 'hello0))
  assert_eq(Vector.get(vec, 5), ('some, 'hello5))
  assert_eq(Vector.get(vec, 1000), ('some, 'hello1000))
  assert_eq(Vector.get(vec, 8000), ('some, 'hello8000))

  assert_eq(Vector.size(vec), 8001)
}