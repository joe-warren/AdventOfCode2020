import System.IO (readFile)
import Data.List (sort)

findAdapterRoute :: [Int] -> (Int, Int)
findAdapterRoute xs = (ones, threes+1)
  where 
    sxs = 0 : sort xs
    steps = (uncurry (-)) <$> (tail sxs) `zip` sxs
    ones = length $ filter (==1) steps
    threes = length $ filter (==3) steps

main :: IO ()
main = do 
    values <- (fmap read) <$> lines <$> readFile "input"
    let res = findAdapterRoute values
    print res
    print $ uncurry (*) res
    
