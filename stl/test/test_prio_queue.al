def test_prio_queue() = {
  let q = PrioQueue.empty()
  let q = q |> PrioQueue.insert(50, 'v50)
  let q = q |> PrioQueue.insert(20, 'v20)
  let q = q |> PrioQueue.insert(30, 'v30_0)
  let q = q |> PrioQueue.insert(10, 'v10)
  let q = q |> PrioQueue.insert(80, 'v80)
  let q = q |> PrioQueue.insert(30, 'v30_1)

  assert_eq(PrioQueue.findMin(q), (10, 'v10))
  let q = q |> PrioQueue.deleteMin()
  assert_eq(PrioQueue.findMin(q), (20, 'v20))
  let q = q |> PrioQueue.deleteMin()
  assert_eq(PrioQueue.findMin(q), (30, 'v30_0))
  let q = q |> PrioQueue.deleteMin()
  assert_eq(PrioQueue.findMin(q), (30, 'v30_1))
}