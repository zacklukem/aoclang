def head(list) = __intrinsic__("ListHead", list)
def tail(list) = __intrinsic__("ListTail", list)
def is(list) = __intrinsic__("ListIs", list)
def is_empty(list) = __intrinsic__("ListIsEmpty", list)
def to_tuple(list) = __intrinsic__("ListToTuple", list)

def len(arr) = _len(arr, 0)

def _len([], acc) = acc
def _len(h :: tail, acc) = _len(tail, acc + 1)

def Enumerable.from(l: List) = l
def Enumerable.next(hd :: tail) = (tail, ('some, hd))
def Enumerable.next([]) = ([], 'none)
