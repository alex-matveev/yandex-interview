import Control.Monad

initial :: IO Int
initial = do
    a <- read <$> getLine
    print a
    return a

duplicatesRemoval :: Int -> Int -> IO()
duplicatesRemoval 0 _ = return ()
duplicatesRemoval t a = do
    b <- read <$> getLine
    unless (a == b) $ print b 
    duplicatesRemoval (t-1) b

main :: IO ()
main = do
    t <- read <$> getLine
    unless (t < 1) $ initial >>= duplicatesRemoval (t-1)
