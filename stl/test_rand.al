def test_next() = {
  let rng = Rand.new_seeded(774899539)
  let n = 1000
  let (rng, total) = Enum.reduceWith(range(0, n), (rng, 0.0), /_, (rng, acc)/ {
    let (rng, rand) = Rand.next_float(rng)
    (rng, rand + acc)
  })
  let avg = total / n
  assert(avg < 0.515)
  assert(avg > 0.485)
}
