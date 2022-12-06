
import System.Environment
import System.IO
import Data.Char
import Data.IntMap (IntMap)
import Data.List (foldl', partition)
import Data.List.Extra (chunksOf)
import Data.List.Split (splitOn, splitWhen)
import qualified Data.IntMap as IM
import Text.Regex.TDFA

parseStacks :: [String] -> IntMap String
parseStacks s = IM.fromList . zip [1..] $ foldl' f (replicate ((length labels + 1) `div` 4) []) rows
  where
    labels:rows = reverse s
    f s row = zipWith (\a b -> let n = b!!1 in if isAlpha n then n:a else a) s (chunksOf 4 row)

parseMoves :: [String] -> [[Int]]
parseMoves moves = map f moves
  where
    f :: String -> [Int]
    f m = map read $ getAllTextMatches (m =~ "[0-9]+")

applyMove :: IntMap String -> [Int] -> IntMap String
applyMove s [n,f,t] = IM.fromList [(f, xs'), (t, ys')] `IM.union` s
  where
    (top, xs') = splitAt n $ s IM.! f
    ys' = reverse top ++ s IM.! t

applyMove2:: IntMap String -> [Int] -> IntMap String
applyMove2 s [n,f,t] = IM.fromList [(f, xs'), (t, ys')] `IM.union` s
  where
    (top, xs') = splitAt n $ s IM.! f
    ys' = top ++ s IM.! t

main :: IO ()
main = do
  args <- getArgs
  file <- readFile $ case args of
         (x:_) -> x
         [] -> "input.txt"
  
  let [stacks', moves'] = splitWhen (== "") $ lines file
      stacks = parseStacks stacks'
      moves = parseMoves moves'
  putStrLn $ show stacks
  putStrLn $ show moves
  putStrLn $ show $ snd $ unzip $ IM.toList $ IM.map head $ foldl' applyMove stacks moves
  putStrLn $ show $ snd $ unzip $ IM.toList $ IM.map head $ foldl' applyMove2 stacks moves
