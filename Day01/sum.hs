import System.IO (readFile)
findPairs :: [Int] -> [(Int, Int)]
findPairs xs = filter ((== 2020) . (uncurry (+))) $ (,) <$> xs <*> xs

main :: IO ()
main = do 
    values <- (fmap read) <$> lines <$> readFile "input"
    let pairs = findPairs values
    print $ uncurry (*) <$> pairs
    
