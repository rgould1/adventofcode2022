import System.Environment
import System.IO
import Data.Ord
import Data.Char
import Data.List.Extra (chunksOf)
import qualified Data.Set as S
import Data.Tuple.Extra (both)

priority :: Char -> Int
priority c = if isUpper c
             then ord c - ord 'A' + 27
             else ord c - ord 'a' + 1

f1 :: [String] -> String
f1 ls = show $ sum $ map (priority . common) ls
  where
    common s = S.findMin $ uncurry S.intersection $ S.fromList `both` sect s
    sect :: String -> (String, String)
    sect s = splitAt (length s `div` 2) s

f2 :: [String] -> String
f2 ls = show $ sum $ map (priority . badge) (chunksOf 3 $ map S.fromList ls)
  where
    badge ls = S.findMin $ foldl1 S.intersection ls

main :: IO ()
main = do
  args <- getArgs
  file <- readFile $ case args of
         (x:_) -> x
         [] -> "input.txt"

  putStrLn $ f1 $ lines file
  putStrLn $ f2 $ lines file
