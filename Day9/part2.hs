import System.IO (readFile)
import Data.List (tails, inits)
import Control.Monad (liftM2)


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
          
sumMinMax :: [Int] -> Int
sumMinMax = liftM2 (+) minimum maximum
      
findSubsequence :: Int -> [Int] -> [Int]
findSubsequence t xs = let s = head $ (filter $ (>=t).sum) $ inits xs in
                        if t == sum s then s
                                      else findSubsequence t (tail xs)
main :: IO ()
main = do 
   values <- (fmap read) <$> lines <$> readFile "input"
   let target = head $ part1 values
   print $ sumMinMax $ findSubsequence target values
    
    
