import Data.Char
import Data.List
import Data.Map qualified as M
import GHCi.Message qualified as M
import Geometry

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

findKey :: (Eq k) => k -> [(k, v)] -> v
findKey key xs = snd . head . filter (\(k, v) -> key == k) $ xs

phoneBook = [("betty", "555-2938"), ("bonnie", "452-2928")]

findKeyV2 :: (Eq k) => k -> [(k, v)] -> Maybe v
findKeyV2 key [] = Nothing
findKeyV2 key ((k, v) : xs)
  | key == k = Just v
  | otherwise = findKeyV2 key xs

findKeyV3 :: (Eq k) => k -> [(k, v)] -> Maybe v
findKeyV3 key xs =
  foldr
    (\(k, v) acc -> if key == k then Just v else acc)
    Nothing
    xs

phoneBookV2 :: M.Map String String
phoneBookV2 = M.fromList $ [("betty", "555-2938"), ("bonnie", "452-2928")]

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
  print $ map firstTo [10, 20 .. 40]
  print $ findKey "bonnie" $ phoneBook
  -- print $ findKey "bonie" $ phoneBook
  print $ findKeyV2 "bonnie" phoneBook
  print $ findKeyV2 "bonie" phoneBook
  print $ findKeyV3 "bonnie" phoneBook
  print $ findKeyV3 "bonie" phoneBook
  print $ M.fromList [(3, "shoes"), (4, "trees"), (9, "bees")]
  print $ phoneBookV2
  print $ M.lookup "bonnie" phoneBookV2
  let newBook = M.insert "grace" "341-9021" phoneBookV2
  print $ M.lookup "grace" newBook
  print $ sphereVolume 2.3
