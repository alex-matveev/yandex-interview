import Control.Monad

merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x < y     = x:merge xs (y:ys)
    | otherwise = y:merge (x:xs) ys

mergeLists :: [[Int]] -> [Int]
mergeLists = foldl merge []

getUserInputs :: Int -> IO [[Int]]
getUserInputs t = replicateM t $ do
    n <- getLine
    return $ tail $ read <$> words n

main :: IO ()
main = do
    k <- read <$> getLine
    lists <- getUserInputs k
    let res = mergeLists lists
    mapM_ (putStrLn . show) res
