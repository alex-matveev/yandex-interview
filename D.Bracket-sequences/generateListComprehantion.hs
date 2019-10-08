import Control.Monad(mapM_)

generate :: Int -> [String]
generate = generate' 0
        where
        generate' _ 0 = [[]]
        generate' a n = [x:xs | x <- possible, xs <- step x]
            where
            step '(' = generate' (a + 1) (n - 1) 
            step ')' = generate' (a - 1) (n - 1)
            possible
                | n == a = ")"
                | a == 0 = "("
                | otherwise = "()"

main :: IO ()
main = do
    n <- read <$> getLine
    let res = generate $ n * 2
    mapM_ putStrLn res
