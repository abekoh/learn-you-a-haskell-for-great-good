import Distribution.Verbosity (normal)

-- {-# OPTIONS -Wall -Werror #-}
lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky _ = "Sorry, you're out of luck, pal!"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- head' :: [a] -> a
-- head' [] = error "Can't call head on an empty list, dummy!"
-- head' (x : _) = x

-- badAdd :: (Num a) => [a] -> a
-- badAdd (x:y:z:[]) = x + y + z

firstLetter :: String -> String
firstLetter "" = "Empty string, whoops!"
firstLetter a@(x : _) = "The first letter of " ++ a ++ " is " ++ [x]

bmiTell :: Double -> Double -> String
bmiTell weight height
  | bmi <= skinny = "You're underweight, you emo, you!"
  | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= fat = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, conguratulations!"
  where
    bmi = weight / height ^ 2
    skinny = 18.5
    normal = 25.0
    fat = 30.0

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where
    bmi weight height = weight / height ^ 2

main :: IO ()
main = do
  -- print (removeNonUppercase "PfwoefhapiuHEW")
  -- print (addThree 1 2 3)
  -- print(factorial 100)
  print (factorial 7)
  print (lucky 8)
  print (addVectors (1.0, 2.0) (3.0, 4.0))
  -- print (head' [1, 2, 3])
  print (firstLetter "Dracula")
  print (bmiTell 54 1.62)
  print (calcBmis [(54, 1.62)])
