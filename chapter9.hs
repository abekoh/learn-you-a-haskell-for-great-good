import Control.Monad
import Data.Char
import System.IO
import System.Environment

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
