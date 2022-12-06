
import System.Environment
import System.IO
import Data.List (nub)

subsequences :: Int -> [a] -> [[a]]
subsequences n [] = []
subsequences n s = take n s : subsequences n (tail s)

marker :: Int -> String -> Int
marker n s = fst $ head $ dropWhile (\(_, a) -> nub a /= a) $ zip [n..] $ subsequences n s

main :: IO ()
main = do
  args <- getArgs
  file <- readFile $ case args of
         (x:_) -> x
         [] -> "input.txt"

  putStrLn $ show $ map (marker 4) $ lines file
  putStrLn $ show $ map (marker 14) $ lines file
