removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <- st, c `elem` ['A' .. 'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

main = do
  print (removeNonUppercase "PfwoefhapiuHEW")
  print (addThree 1 2 3)
