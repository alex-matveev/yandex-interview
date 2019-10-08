import Control.Monad

duplicatesRemoval :: [Int] -> [Int]
duplicatesRemoval (x:xs) = x:duplicatesRemoval' x xs
    where
        duplicatesRemoval' _ [] = []
        duplicatesRemoval' prev (x:xs)
            | prev /= x = x:(duplicatesRemoval' x xs)
            | otherwise = duplicatesRemoval' x xs

getUserInputs :: Int -> IO [Int]
getUserInputs t = replicateM t $ read <$> getLine

main :: IO ()
main = do
    t <- read <$> getLine
    unless (t < 1) $ (duplicatesRemoval <$> getUserInputs t) >>= mapM_ print

