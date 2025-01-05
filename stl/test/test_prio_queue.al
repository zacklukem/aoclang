def test_prio_queue() = {
  let q = PrioQueue.new()
  let q = q |> PrioQueue.enqueue(50, 'v50)
  let q = q |> PrioQueue.enqueue(20, 'v20)
  let q = q |> PrioQueue.enqueue(30, 'v30_0)
  let q = q |> PrioQueue.enqueue(10, 'v10)
  let q = q |> PrioQueue.enqueue(80, 'v80)
  let q = q |> PrioQueue.enqueue(30, 'v30_1)

  let expected = [
     (10, 'v10),
     (20, 'v20),
     (30, 'v30_0),
     (30, 'v30_1),
     (50, 'v50),
     (80, 'v80),
  ]

  assert_eq(Enum.to_list(q), expected)

  let q = expected |> Enum.reduceWith(q, /expected, q/ {
    let (q, actual) = q |> PrioQueue.dequeue()
    assert_eq(actual, expected)
    q
  })

  assert(PrioQueue.is_empty(q))
  assert_eq(PrioQueue.min(q), 'none)
  assert(!PrioQueue.is(5))
}