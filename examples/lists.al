
def main() = {
  println("Length of nil: " ++ len('nil))
  println("Length of 1: " ++ len(0 :: 'nil))
  println("Length of 2: " ++ len(1 :: (0 :: 'nil)))

  println("Range of 0 to 0: " ++ range(0, 0))
  println("Range of 0 to 5: " ++ range(0, 5))

  foreach(range(0, 5), println)
}
