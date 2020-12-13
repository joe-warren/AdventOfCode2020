{-# LANGUAGE RankNTypes #-}
import System.IO (readFile)
import Control.Lens
import Numeric.Lens
import Data.Bool (bool)
import Data.Maybe (fromJust)
import Data.List ((\\))

exchange :: Eq a => a -> a -> Iso' a a 
exchange a b = iso go go
  where
    go x | a == x = b
         | b == x = a
         | otherwise = x

rowPos :: String -> (Int, Int)
rowPos s = (r, c)
  where
    parseBinary :: Char -> Char -> String -> Int
    parseBinary t f = fromJust . preview (mapping (exchange t '1' . exchange f '0') . binary)
    r = parseBinary 'B' 'F' $ take 7 s
    c = parseBinary 'R' 'L' $ take 3 $ drop 7 s

seatId :: (Int, Int) -> Int
seatId (r, c) = r*8 +c

mySeat :: [Int] -> [Int]
mySeat ss = (enumFromTo (minimum ss) (maximum ss)) \\ ss

main :: IO ()
main = do 
    values <- lines <$> readFile "input"
    let ids = seatId . rowPos <$> values
    print $ mySeat ids
    
