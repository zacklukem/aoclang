def new_seeded(seed) = {
  ('rng, seed, seed)
}

def next(('rng, state, inc)) = {
  let state' = (state * 6364136223846793005) + (inc | 1)

  let mask = 4294967295
  let xorshifted = (((state >> 18) ^ state) >> 27) & mask
  let rot = (state >> 59) & mask
  let rand = (xorshifted >> rot) | ((xorshifted << ((0-rot) & 31)) & mask)

  (('rng, state', inc), rand & mask)
}

def next_float(rng) = {
  let (rng, rand) = next(rng)
  (rng, rand / 4294967295)
}
