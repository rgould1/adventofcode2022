import System.IO
import Data.List

f1 :: [String] -> String
f1 ls = show $ fst $ foldl f (0, 0) ls
  where
    f (m, c) s = if s == ""
                 then (max m c, 0)
                 else (m, c + read s)

f2 :: [String] -> String
f2 ls = show $ sum $ fst $ foldl f ([], 0) ls
  where
    f (m, c) s = if s == ""
                 then (take 3 $ reverse $ sort $ c:m, 0)
                 else (m, c + read s)

main :: IO ()
main = do
  file <- readFile "input.txt"

  putStrLn $ f1 $ lines file
  putStrLn $ f2 $ lines file
