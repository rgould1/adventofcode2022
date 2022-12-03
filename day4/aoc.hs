import System.Environment
import System.IO
import Data.List (partition)
import Data.List.Extra (chunksOf)
import Data.List.Split (splitOn)

pair :: String -> [String]
pair = splitOn ","

tup :: [a] -> (a, a)
tup x = (x !! 0, x !! 1)

sections :: String -> (Int, Int)
sections s = tup $ map read $ splitOn "-" s

-- Test if first section fully contains second
contains :: (Int, Int) -> (Int, Int) -> Bool
contains (as, ae) (bs, be) = as <= bs && ae >= be

overlap :: (Int, Int) -> (Int, Int) -> Bool
overlap (as, ae) (bs, be) = as <= bs && bs <= ae || as <= be && be <= ae

symetric f a b = a `f` b || b `f` a 

f1 :: String -> Bool
f1 s = uncurry (symetric contains) $ tup $ map sections $ pair s

f2 :: String -> Bool
f2 s = uncurry (symetric overlap) $ tup $ map sections $ pair s

main :: IO ()
main = do
  args <- getArgs
  file <- readFile $ case args of
         (x:_) -> x
         [] -> "input.txt"

  putStrLn $ show $ length $ filter f1 $ lines file
  putStrLn $ show $ length $ filter f2 $ lines file
