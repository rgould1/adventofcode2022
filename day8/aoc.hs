
import System.Environment
import System.IO
import If
import Control.Monad
import Control.Monad.Loops
import Control.Monad.Except (liftEither, liftIO, runExceptT, ExceptT(..))
import Numeric.Matrix (Matrix)
import qualified Numeric.Matrix as M
import Control.Arrow ((***))
import Data.Char
import Data.Composition
import Data.Either
import Data.List (foldl', foldl1', sort, span)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Monoid
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as Set

type Error a = Either String a
type ErrorIO a = ExceptT String IO a

main :: IO ()
main = do
  r <- runExceptT process
  printReport r

hover :: Matrix Int -> (Int, Int) -> Int -> (Bool, Int)
hover m (r, c) height = foldl1' (\(a, b) (a', b') -> (a || a', b * b')) [top, left, bottom, right]
  where
      acc f (v, s) i = (v && m `M.at` f i < height, s + (v ? 1 $ 0))
      accv = acc $ \r' -> (r', c)
      acch = acc $ \c' -> (r, c')
      top = foldl' accv (True, 0) [r-1,r-2..1]
      left = foldl' acch (True, 0) [c-1,c-2..1]
      bottom = foldl' accv (True, 0) [r+1,r+2..M.numRows m]
      right = foldl' acch (True, 0) [c+1,c+2..M.numCols m]

process :: ErrorIO ()
process = do 
  args <- liftIO getArgs
  file <- liftIO $ readFile $ case args of
         (x:_) -> x
         [] -> "input.txt"

  let grid = lines file
      width = length $ head grid
      height = length grid
      matrix = M.fromList [[digitToInt col | col <- row] | row <- grid]
      --sm = M.mapWithIndex (snd .: hover matrix) matrix
      (visible, scores) = M.foldMapWithIndex ((Sum . (\s -> s ? 1 $ 0) *** Max) .: hover matrix) matrix
  --liftIO $ putStrLn $ show sm
  liftIO $ putStrLn $ show $ getSum visible
  liftIO $ putStrLn $ show $ getMax scores

  return ()
  
printReport :: Error () -> IO ()
printReport (Left err) = putStrLn ("Failed with error: " ++ err)
printReport (Right _) = do
  putStrLn "Finished"
