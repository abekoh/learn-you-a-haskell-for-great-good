-- {-# OPTIONS -Wall -Werror #-}

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list!"
maximum' [x] = x
maximum' (x : xs) = max x (maximum' xs)

replicate' :: Int -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x : replicate' (n - 1) x

take' :: Int -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x : xs) = x : take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x : xs)
  | a == x = True
  | otherwise = elem' a xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let smallerOrEqual = [a | a <- xs, a <= x]
      larger = [a | a <- xs, a > x]
   in quicksort smallerOrEqual ++ [x] ++ quicksort larger

main :: IO ()
main = do
  print (maximum' [1, 45, 20, 10, 33])
  print (replicate' 10 5)
  print (take' 3 [1, 2, 3, 4, 5, 6])
  print (reverse' [1, 2, 3, 4, 5])
  print (elem' 2 [1, 2, 3, 4, 5])
  print (elem' 0 [1, 2, 3, 4, 5])
  print (quicksort [9, 1, 2, 5, 8, 0, 3])
  print (quicksort "the quick brown fox jumps over the lazy dog")
