def add1(x) = x + 1

def main() = {
  println("Length of nil: " ++ len('nil))
  println("Length of 1: " ++ len(0 :: 'nil))
  println("Length of 2: " ++ len([1, 0]))

  println("Range of 0 to 0: " ++ range(0, 0))
  println("Range of 0 to 5: " ++ range(0, 5))
  println("List of 0 to 5: " ++ [0, 1, 2, 3, 4, 5])

  println("'nil == []: " ++ ('nil == []))
  println("[1] == [1]: " ++ ([1] == [1]))

  println("Add1 Range of 0 to 5: " ++ map(range(0, 5), add1))
  println("Rev Range of 0 to 5: " ++ rev(range(0, 5)))

  foreach(range(0, 5), println)
}
