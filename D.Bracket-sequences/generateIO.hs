import Control.Monad(mapM_)

generate :: Int -> IO()
generate = generate' "" 0
        where
        generate' xs _ 0 = putStrLn $ reverse xs
        generate' xs a n 
            | n == a = step ')'
            | a == 0 = step '('
            | otherwise = step '(' >> step ')'
            where
                step '(' = generate' ('(':xs) (a + 1) (n - 1) 
                step ')' = generate' (')':xs) (a - 1) (n - 1)


main :: IO ()
main = do
    n <- read <$> getLine
    generate $ n * 2
