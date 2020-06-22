type Name = String
type Age = Int
data Student = Student Name Age
  deriving Eq

main :: IO ()
main = do
  let st1 = Student "Jane Doe" 20
      st2 = Student "John Doe" 22
  print $ st1 == st2
  print $ st1 /= st2
