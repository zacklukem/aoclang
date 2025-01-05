def fibrec(a, b, 0) = 'none
def fibrec(a, b, n) = {
    println("fib: " ++ b)
    fibrec(b, a + b, n - 1)
}

def fib(k) = fibrec(0, 1, k)

def main() = {
  println(fib(1000))
}
