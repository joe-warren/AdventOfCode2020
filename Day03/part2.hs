import System.IO (readFile)

countTrees :: [String] -> Int -> Int -> Int
countTrees s r d = length $ filter (uncurry test) $ 
    zip [1..] $
    fmap snd $
    filter ((==0) . (`mod` d) . fst) $
    zip [1..] (cycle <$> s)
  where
    test :: Int -> String -> Bool
    test i s = (s !! (i*r)) == '#'

countSlopes :: [String] -> Int
countSlopes s = product $ uncurry (countTrees s) <$> [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

main :: IO ()
main = do
    content <- tail . lines <$> readFile "input"
    print $ countSlopes content
    
