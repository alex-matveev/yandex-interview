import Control.Monad
import Data.Array.IO
import Data.Array.Base

addToIOArray :: IOUArray Int Int -> Int -> IO ()
addToIOArray arr idx = do
  val <- unsafeRead arr idx
  unsafeWrite arr idx $ val + 1

fillIOArray :: IOUArray Int Int -> IO ()
fillIOArray arr = ((tail.map read.words) <$> getLine) >>= mapM_ (addToIOArray arr)

printPair :: Show a => (a, Int) -> IO ()
printPair (a, b) = replicateM_ b $ print a

main :: IO ()
main = do
    k <- read <$> getLine
    arr <- newArray (0,100) 0 :: IO (IOUArray Int Int)
    replicateM k $ fillIOArray arr
    getAssocs arr >>= mapM_ printPair
