import Control.Monad
import Data.Array.Unboxed
import Data.Array.Base

addToArray :: UArray Int Int -> [Int] -> UArray Int Int
addToArray acc elems = unsafeAccum (+) acc [(i, 1) | i<-elems]

foldM' :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM' _ z [] = return z
foldM' f z (x:xs) = do
    z' <- f z x
    z' `seq` foldM'' f z' xs

printPair :: Show a => (a, Int) -> IO ()
printPair (a, b) = replicateM_ b $ print a

main :: IO ()
main = do
    k <- read <$> getLine
    let init = array (0, 100) [(i, 0) | i <- [0..100]]
    res <- foldM' ((<$>) . addToArray) init $ replicate k $ (tail.map read.words) <$> getLine
    mapM_ printPair $ assocs res
