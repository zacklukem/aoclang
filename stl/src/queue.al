def is(('queue, in, out)) = 1 == 1
def is(_) = 1 == 0

def new(vs) = {
  ('queue, vs, [])
}

def new_empty() = new([])

def enqueue(('queue, in, out), value) = {
  ('queue, value :: in, out)
}

def dequeue(('queue, [], [])) = (('queue, [], []), 'none)
def dequeue(('queue, in, v :: tail)) = (('queue, in, tail), ('some, v))
def dequeue(('queue, in, [])) = dequeue(('queue, [], Enum.rev(in)))

def Enumerable.from(q: Queue) = q
def Enumerable.next(('queue, in, out)) = dequeue(('queue, in, out))
