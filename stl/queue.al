def new(vs) = {
  ('queue, vs, [])
}

def new_empty() = new([])

def enqueue(('queue, in, out), value) = {
  ('queue, value :: in, out)
}

def dequeue(('queue, [], [])) = (('queue, [], []), 'none)
def dequeue(('queue, in, v :: tail)) = (('queue, in, tail), v)
def dequeue(('queue, in, [])) = dequeue(('queue, [], rev(in)))
