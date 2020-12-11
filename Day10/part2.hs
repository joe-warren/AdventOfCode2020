import System.IO (readFile)
import Data.List (sort)
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import qualified Data.IntSet as IntSet

findAdapterRoute :: [Int] -> Int
findAdapterRoute xs = countSlns 0
  where 
    target = 3 + maximum xs
    sxs = 0 : sort xs ++ [target]
    countSlns' :: Int -> Int
    countSlns' x1 = 
        let l = tail $ dropWhile (/=x1) sxs
        in
           case l of
                [] -> 1
                _ -> sum $ do
                    gap <- [1..3]
                    if elem (x1 + gap) $ take gap l
                        then return $ countSlns (x1 + gap)
                        else return 0
    s = IntSet.fromList sxs
    m = IntMap.fromSet countSlns' s

    countSlns :: Int -> Int
    countSlns x = m IntMap.! x
main :: IO ()
main = do 
    values <- (fmap read) <$> lines <$> readFile "input"
    let res = findAdapterRoute values
    print res
