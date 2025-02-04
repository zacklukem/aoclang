
def test_queue() = {
  let q = Queue.new([1, 2, 3])
  let (q, ('some, v)) = Queue.dequeue(q)
  assert(v == 3)
  let (q, ('some, v)) = Queue.dequeue(q)
  assert(v == 2)
  let q = Queue.enqueue(q, 66)
  let (q, ('some, v)) = Queue.dequeue(q)
  assert(v == 1)
  let (q, ('some, v)) = Queue.dequeue(q)
  assert(v == 66)
  let (_, 'none) = Queue.dequeue(q)
}