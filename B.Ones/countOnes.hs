import Control.Monad (replicateM)

onesCount :: [Char] -> Int
onesCount xs = onesCount' xs 0 0
    where
        onesCount' "" max curr 
            | max > curr = max 
            | otherwise  = curr
        onesCount' (x:xs) max curr
            | x == '1' = onesCount' xs max $ curr + 1 
            | curr > max = onesCount' xs curr 0 
            | otherwise = onesCount' xs max 0

getUserInputs :: IO [Char]
getUserInputs = do
    n <- read <$> getLine :: IO Int
    replicateM n $ head <$> getLine

main :: IO ()
main = do
    xs <- getUserInputs 
    print $ onesCount xs

