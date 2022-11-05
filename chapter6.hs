import Data.Char
import Data.List
import Data.Map qualified as M

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)

encode :: Int -> String -> String
encode offset msg = map (\c -> chr $ ord c + offset) msg

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

firstTo :: Int -> Maybe Int
firstTo n = find (\x -> digitSum x == n) [1 ..]

main :: IO ()
main = do
  print $ numUniques [1, 1, 2, 3, 3, 4, 5]
  print $ words "hoge fuga foo bar"
  print $ sort [5, 3, 5, 2, 3]
  print $ "art" `isIn` "party"
  print $ ord 'a'
  print $ ord 'A'
  print $ ord 'あ'
  print $ chr 100
  print $ encode 12 "I'm abekoh."
  print $ decode 12 . encode 12 $ "I'm abekoh."
  print $ foldl' (+) 0 (replicate 1000000 1)
  print $ find (> 4) [3, 4, 5, 6, 7]
  print $ find odd [3, 4, 5, 6, 7]
  print $ find (== 'z') "mjolnir"
  print $ digitToInt 'F'
  print $ digitSum 123
  print $ find (\x -> digitSum x == 40) [1 ..]
  print $ firstTo 13
  print $ map firstTo [10, 20 .. 60]
