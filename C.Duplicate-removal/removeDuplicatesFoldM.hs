import Control.Monad(unless, foldM_)

step :: Int -> Int -> IO Int
step fst snd = unless (fst == snd) (print snd) >> return snd

initial :: IO Int
initial = do
    a <- read <$> getLine
    print a
    return a

getUserInputs t = replicate t (read <$> getLine)

main :: IO ()
main = do
    t <- read <$> getLine
    unless (t < 1) $ do
        init <- initial
        foldM_ ((=<<) . step) init $ getUserInputs (t-1)
