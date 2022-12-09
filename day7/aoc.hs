
import System.Environment
import System.IO
import Control.Monad
import Control.Monad.Loops
import Control.Monad.Except (liftEither, liftIO, runExceptT, ExceptT(..))
import Data.Char
import Data.Either
import Data.List (break, find, foldl', sort, span)
import Data.List.Extra (snoc)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Item = Folder String [Item] | File String Int deriving (Show)
data Crumb = Crumb String [Item] [Item] deriving (Show)
type Zipper = (Item, [Crumb])
type Error a = Either String a
type ErrorIO a = ExceptT String IO a

printTree :: Zipper -> IO ()
printTree z = print' 0 "" m z
  where
    m = snd $ fromRight (0, Map.empty) (size z)
    print' :: Int -> String -> Map String Int -> Zipper -> IO ()
    print' d path m i@(Folder folderName _, bs) =
      do
        let p = path ++ "->" ++ folderName
            s = maybe "Missing!" show (m Map.!? p)
        putStrLn $ replicate (2*d) ' ' ++ "-" ++ folderName ++ "(dir, size=" ++ s ++ ")"
        void $ iterateUntilM isNothing (\(Just n) -> print' (d+1) p m n >> return (next n)) (first i)
    print' d _ _ (File fileName sz, _) = putStrLn $ replicate (2*d) ' ' ++ "-" ++ fileName ++ "(file, size=" ++ show sz ++ ")"

up :: Zipper -> Error Zipper  
up (item, []) = fail "called up at root"
up (item, Crumb name ls rs:bs) = return (Folder name (ls ++ [item] ++ rs), bs)

root :: Zipper -> Error Zipper
root = iterateUntilM (null . snd) up

to :: String -> Zipper -> Error Zipper  
to name (Folder folderName items, bs) =   
    case break (nameIs name) items of
      (_, []) -> fail $ "to unfound dir: " ++ name
      (ls, item:rs) -> return (item, Crumb folderName ls rs:bs)  

first :: Zipper -> Maybe Zipper
first (Folder folderName [], bs) = Nothing
first (Folder folderName (item:rs), bs) = Just (item, Crumb folderName [] rs:bs)
first (File _ _, _) = Nothing

next :: Zipper -> Maybe Zipper
next (_, Crumb folderName _ []:_) = Nothing
next (item, Crumb folderName ls (n:rs):bs) = Just (n, Crumb folderName (ls `snoc` item) rs:bs)
  
nameIs :: String -> Item -> Bool  
nameIs name (Folder folderName _) = name == folderName  
nameIs name (File fileName _) = name == fileName

nameOf :: Item -> String  
nameOf (Folder folderName _) = folderName  
nameOf (File fileName _) = fileName

newFile :: Item -> Zipper -> Error Zipper  
newFile i (File name _, _) = error $ "Tried to add newFile (" ++ nameOf i ++ ") to file: " ++ name
newFile item (Folder folderName items, bs) =   
    return (Folder folderName (item:items), bs)

size :: Zipper -> Error (Int, Map String Int)
size = size' ""
  where
    size' :: String -> Zipper -> Error (Int, Map String Int)
    size' path d@(Folder s items, _) = do
      let 
        p = path ++ "->" ++ s
        f (sz, m) i = do
                     (s', m') <- to (nameOf i) d >>= size' p 
                     return (s' + sz, m' `Map.union` m) 
      (sz, m) <- foldM f (0, Map.empty) items
      return (sz, Map.insert p sz m)
    size' _ f@(File _ sz, _) = return (sz, Map.empty)

runCommand :: Zipper -> [String] -> Error (Zipper, [String])
runCommand node [] = fail "no command"
runCommand node (cmd:rest) =
  case (take 4 cmd) of
       "$ cd" -> do
                   res <- case (drop 5 cmd) of
                            "/" -> root node
                            ".." -> up node
                            x -> to x node
                   return (res, rest)
       "$ ls" -> iterateUntilM (\(_, cs) -> null cs || head (head cs) =='$')
                               (\(f, c:rest) -> do
                                                  let (s, _:l) = span (not . isSpace) c
                                                  res <- if s == "dir"
                                                         then newFile (Folder l []) f
                                                         else newFile (File l (read s)) f
                                                  return (res, rest))
                               (node, rest)
       x -> fail $ "Unexpected command: " ++ x

main :: IO ()
main = do
  r <- runExceptT process
  printReport r

  
process :: ErrorIO (Int, Map String Int)
process = do 
  args <- liftIO getArgs
  file <- liftIO $ readFile $ case args of
         (x:_) -> x
         [] -> "input.txt"

  let commands = lines file
      empty = (Folder "/" [], [])
  (run, _) <- liftEither $ iterateUntilM (null . snd) (uncurry runCommand) (empty, commands)
  tree <- liftEither $ root run
  r <- liftEither $ size tree
  liftIO $ printTree tree
  return r
  
printReport :: Error (Int, Map String Int) -> IO ()
printReport (Left err) = putStrLn ("Failed with error: " ++ err)
printReport (Right (ts, sz)) = do
  let folderSizes = snd $ unzip $ Map.toList sz 
      total = 70000000
      required = 30000000
      available = total - ts
      needed = required - available
      result1 = sum $ filter (<= 100000) folderSizes 
      result2 = find (> needed) $ sort folderSizes
  putStrLn $ show result1
  putStrLn $ "Used: " ++ show ts ++ ", Needed: " ++ show needed ++ ", Smallest to delete: " ++ show result2
