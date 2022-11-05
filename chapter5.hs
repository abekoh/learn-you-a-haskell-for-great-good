-- {-# OPTIONS -Wall -Werror #-}

multThree :: Int -> Int -> Int -> Int
multThree x y z = x * y * z

compareWithHundred :: Int -> Ordering
compareWithHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/ 10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A' .. 'Z'])

subtractFour :: (Num a) => a -> a
subtractFour = subtract 4

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
-- flip' f = g
--   where
--     g x y = f y x
flip' f y x = f x y

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x : xs) =
  let smallerOrEqual = [a | a <- xs, a <= x]
      larger = [a | a <- xs, a > x]
   in quicksort' smallerOrEqual ++ [x] ++ quicksort' larger

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let smallerOrEqual = filter (<= x) xs
      larger = filter (> x) xs
   in quicksort smallerOrEqual ++ [x] ++ quicksort larger

largestDivisible :: Integer
largestDivisible = head (filter p [100000, 99999 ..])
  where
    p x = x `mod` 3829 == 0

chain :: Int -> [Int]
chain 1 = [1]
chain x
  | even x = x : chain (x `div` 2)
  | otherwise = x : chain (x * 3 + 1)

longChains :: [[Int]]
longChains = filter isLong (map chain [1 .. 100])
  where
    isLong xs = length xs > 15

numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1 .. 100]))

main :: IO ()
main = do
  -- let multTwoWithNine = multThree 9
  -- print (multTwoWithNine 2 3)
  -- print (compareWithHundred 99)
  -- print (divideByTen 200)
  -- print (isUpperAlphanum 'c')
  -- print (isUpperAlphanum 'Q')
  -- print (isUpperAlphanum 'Q')
  -- print (subtractFour 10)
  -- print (multThree 3 4)
  -- print (applyTwice (+ 3) 10)
  -- print (applyTwice (multThree 2 2) 9)
  -- print (zipWith' (+) [1, 2, 3] [4, 5, 6])
  -- print (zipWith' (zipWith' (*)) [[1, 2, 3], [3, 5, 6]] [[3, 2, 2], [3, 4, 5], [5, 4, 3]])
  -- print (zip [1, 2, 3, 4, 5] "hello")
  -- print (flip' zip [1, 2, 3, 4, 5] "hello")
  print (map (+ 3) [1, 5, 3, 1, 6])
  print (map (* 3) [1, 5, 3, 1, 6])
  print (map (map (^ 2)) [[1, 2], [3, 4, 5, 6], [7, 8]])
  print (filter (> 2) [1, 5, 1, 3, 9, 8, 9])
  print (let notNull x = not (null x) in filter notNull [[1, 2, 3], [], [3, 4, 5], [2, 2], [], [], []])
  print (quicksort [2, 9, 8, 0, 1, 8])
  print (quicksort' [2, 9, 8, 0, 1, 8])
  print largestDivisible
  print (sum (takeWhile (< 10000) (filter odd (map (^ 2) [1 ..]))))
  print (sum (takeWhile (< 10000) [m | m <- [n ^ 2 | n <- [1 ..]], odd m]))
  print (chain 10)
  print (map head longChains)
  print (length longChains)
  let listOfFuns = map (*) [0 ..]
  print ((listOfFuns !! 4) 5)
  print ([100, 99 ..] !! 5)
  print numLongChains
  print (zipWith (\a b -> (a * 30 + 3) / b) [5, 4, 3, 2, 1] [1, 2, 3, 4, 5])
  print (map (\(a, b) -> a + b) [(1, 2), (3, 5), (6, 3), (2, 6), (2, 5)])
