jeweleryCount :: String -> String -> Int
jeweleryCount j = foldr ((+).(elemInt j)) 0
                where elemInt s x = fromEnum $ elem x s

main :: IO ()
main = do
    j <- getLine
    s <- getLine
    print $ jeweleryCount j s
