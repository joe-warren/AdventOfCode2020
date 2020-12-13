import System.IO (readFile)

countTrees :: [String] -> Int
countTrees s = length $ filter (uncurry test) $ zip [1..] (cycle <$> s)
  where
    test :: Int -> String -> Bool
    test i s = (s !! (i*3)) == '#'

main :: IO ()
main = do
    content <- tail . lines <$> readFile "input"
    print $ countTrees content
    
