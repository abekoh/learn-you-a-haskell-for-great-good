import Control.Monad
-- import System.Random

import Data.ByteString qualified as S
import Data.ByteString.Lazy qualified as B
import Data.Char
import System.Environment
import System.IO

-- threeCoins :: StdGen -> (Bool, Bool, Bool)
-- threeCoins gen =
--   let (firstCoin, newGen) = random gen
--       (secondCoin, newGen') = random newGen
--       (thirdCoin, newGen'') = random newGen'
--    in (firstCoin, secondCoin, thirdCoin)

main :: IO ()
-- main = forever $ do
--   l <- getLine
--   putStrLn $ map toUpper l

-- main = do
--   contents <- getContents
--   putStr $ map toUpper contents

-- main = do
--   handle <- openFile "baabaa.txt" ReadMode
--   contents <- hGetContents handle
--   putStr contents
--   hClose handle

-- main = do
--   withFile "baabaa.txt" ReadMode $ \handle -> do
--     contents <- hGetContents handle
--     putStr contents

-- main = do
--   contents <- readFile "baabaa.txt"
--   putStr contents

-- main = do
--   contents <- readFile "baabaa.txt"
--   writeFile "baabaacaps.txt" (map toUpper contents)

main = do
  args <- getArgs
  progName <- getProgName
  putStrLn "The arguments are:"
  mapM_ putStrLn args
  putStrLn "The program name is:"
  putStrLn progName

  -- print $ threeCoins (mkStdGen 21)
  -- print $ threeCoins (mkStdGen 22)
  -- print $ threeCoins (mkStdGen 943)
  -- print $ threeCoins (mkStdGen 944)
  -- gen <- getStdGen
  -- putStrLn $ take 20 (randomRs ('a', 'z') gen)
  -- gen <- getStdGen
  -- putStrLn $ take 20 (randomRs ('a', 'z') gen)
  -- gen' <- newStdGen
  -- putStrLn $ take 20 (randomRs ('a', 'z') gen')
  -- gen <- getStdGen
  -- putStrLn $ take 20 (randomRs ('a', 'z') gen)
  -- gen <- getStdGen
  -- putStrLn $ take 20 (randomRs ('a', 'z') gen)
  -- gen' <- newStdGen
  -- putStrLn $ take 20 (randomRs ('a', 'z') gen')
  -- gen' <- newStdGen
  -- putStrLn $ take 20 (randomRs ('a', 'z') gen')

  print $ B.pack [99, 97, 110]
  print $ B.pack [98 .. 120]

  let by = B.pack [98, 111, 114, 116]
  print by
  print $ B.unpack by

  print $ B.fromChunks [S.pack [40, 41, 42], S.pack [43, 44, 45], S.pack [46, 47, 48]]
  print $ B.cons 85 $ B.pack [80, 81, 82, 84]
