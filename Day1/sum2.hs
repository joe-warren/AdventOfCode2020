import System.IO (readFile)
import Control.Lens

findPairs :: [Int] -> [(Int, Int,Int)]
findPairs xs = filter ((== 2020) . (foldlOf each (+) 0)) $ (,,) <$> xs <*> xs <*> xs

main :: IO ()
main = do 
    values <- (fmap read) <$> lines <$> readFile "input"
    let pairs = findPairs values
    print $ (foldlOf each (*) 1) <$> pairs
    
