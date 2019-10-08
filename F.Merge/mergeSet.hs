import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Control.Monad
import Data.Foldable

mergeLists :: Set.Set (Int, Int) -> Seq.Seq [Int] -> IO ()
mergeLists set seq 
    | Set.null set = return ()
    | otherwise  = do
        let ((val, idx), set') = Set.deleteFindMin set
        print val
        if null (Seq.index seq idx)
            then mergeLists set' seq
            else mergeLists (Set.insert (head (Seq.index seq idx), idx) set') (Seq.adjust tail idx seq)

getUserInputs :: Int -> IO [[Int]]
getUserInputs t = replicateM t $ do
    n <- getLine
    return $ tail $ read <$> words n

main :: IO ()
main = do
    k <- read <$> getLine
    lists <- getUserInputs k
    let init_seq = Seq.fromList (filter (not . null) lists)
    let init_heap = Set.fromList (zipWith (,) (toList (head <$> init_seq)) [0..])
    mergeLists init_heap $ tail <$> init_seq
