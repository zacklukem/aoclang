def add1(x) = x + 1

# asdf
def main() = {
  println("Length of nil: " ++ List.len([]))
  println("Length of 1: " ++ List.len(0 :: []))
  println("Length of 2: " ++ List.len([1, 0]))

  println("Range of 0 to 0: " ++ range(0, 0))
  println("Range of 0 to 5: " ++ range(0, 5))
  println("List of 0 to 5: " ++ [0, 1, 2, 3, 4, 5])

  println("[1] == [1]: " ++ ([1] == [1]))

  println("Add1 Range of 0 to 5: " ++ Enum.map(range(0, 5), add1))
  println("Rev Range of 0 to 5: " ++ Enum.rev(range(0, 5)))

  Enum.foreach(range(0, 5), println)
}
