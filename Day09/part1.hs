import System.IO (readFile)
import Data.List (tails)


part1 :: [Int] -> [Int]
part1 = (head <$>) . (filter isIllegal) . tails . reverse
  where 
    isIllegal :: [Int] -> Bool
    isIllegal (x:xs) = 
        let l = take 25 xs in
            if length l < 25 
                then False
                else not $ elem x ( (uncurry (+) <$>) $ filter (uncurry (/=)) $ (,) <$> l <*> l )
    isIllegal [] = False
                
main :: IO ()
main = do 
    values <- (fmap read) <$> lines <$> readFile "input"
    print $ part1 values
    
    
