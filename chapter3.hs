{-# OPTIONS -Wall -Werror #-}
lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky _ = "Sorry, you're out of luck, pal!"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

main :: IO ()
main = do
  -- print (removeNonUppercase "PfwoefhapiuHEW")
  -- print (addThree 1 2 3)
  -- print(factorial 100)
  print(factorial 7)
  print(lucky 8)
