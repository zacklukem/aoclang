def fibrec(a, b, n) = {
  if n != 0 {
    println("fib: " ++ b)
    fibrec(b, a + b, n - 1)
  }
}

def fib(k) = fibrec(0, 1, k)

def main() = {
  1000 |> fib |> println
}
