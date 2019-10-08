import Data.Array

hist :: (Ix a, Num b) => (a,a) -> [a] -> Array a b
hist bnds is = accumArray (+) 0 bnds [(i, 1) | i<-is, inRange bnds i]

main :: IO ()
main = do 
    arr1 <- hist ('a','z') <$> getLine
    arr2 <- hist ('a','z') <$> getLine
    if (arr1 == arr2) 
        then print 1 
        else print 0
