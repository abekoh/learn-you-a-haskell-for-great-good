-- import Data.Char
import Control.Monad

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

main :: IO ()
main = do
  -- putStrLn "Hello, what's your first name?"
  -- firstName <- getLine
  -- putStrLn "What's your last name?"
  -- lastName <- getLine
  -- let bigFirstName = map toUpper firstName
  --     bigLastName = map toUpper lastName
  -- putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"
  -- line <- getLine
  -- if null line
  --   then return ()
  --   else do
  --     putStrLn $ reverseWords line
  --     main

  -- return ()
  -- return "HAHAHA"
  -- line <- getLine
  -- return "BLAH BLAH BLAH"
  -- return 4
  -- putStrLn line

  -- input <- getLine
  -- when (input == "SWORDFISH") $ do
  --   putStrLn input

  -- sequence $ map print [1, 2, 3, 4, 5]
  -- return ()

  mapM_ print [1, 2, 3]
