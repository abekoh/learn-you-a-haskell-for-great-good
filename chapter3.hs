{-# OPTIONS -Wall -Werror #-}
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <- st, c `elem` ['A' .. 'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

main = do
  -- print (removeNonUppercase "PfwoefhapiuHEW")
  -- print (addThree 1 2 3)
  -- print(factorial 100)
  print(factorial 7)
