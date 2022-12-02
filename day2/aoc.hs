import System.Environment
import System.IO
import Data.List
import Data.HashMap

f1 :: [String] -> String
f1 ls = show $ foldl f 0 ls
  where
    f t s = t + score (s!!0) (s!!2)
    score a b = p ! b + case (p ! b - p ! a + 3) `mod` 3 of
                          0 -> 3 -- draw
                          1 -> 6 -- win
                          2 -> 0 -- loss
    p = fromList [('A', 1), ('B', 2), ('C', 3), ('X', 1), ('Y', 2), ('Z', 3)]

f2 :: [String] -> String
f2 ls = show $ foldl f 0 ls
  where
    f t s = t + score (s!!0) (s!!2)
    score a b = case b of
                  'X' -> (p ! a + 1) `mod` 3 + 1  -- lose
                  'Y' -> p ! a + 3  -- draw
                  'Z' -> p ! a `mod` 3 + 1 + 6  -- win
    p = fromList [('A', 1), ('B', 2), ('C', 3)]

main :: IO ()
main = do
  args <- getArgs
  file <- readFile $ case args of
         (x:_) -> x
         [] -> "input.txt"

  putStrLn $ f1 $ lines file
  putStrLn $ f2 $ lines file
