import System.IO (readFile)
import Control.Lens
import Numeric.Lens
import Data.Bool (bool)
import Data.Maybe (fromJust)
import Data.List ((\\))

rowPos :: String -> (Int, Int)
rowPos s = (r, c)
  where
    parseBinary :: Char -> String -> Int
    parseBinary c = fromJust . preview binary . (fmap $ (bool '0' '1') . (==c)) 
    r = parseBinary 'B' $ take 7 s
    c = parseBinary 'R' $ take 3 $ drop 7 s

seatId :: (Int, Int) -> Int
seatId (r, c) = r*8 +c

mySeat :: [Int] -> [Int]
mySeat ss = (enumFromTo (minimum ss) (maximum ss)) \\ ss

main :: IO ()
main = do 
    values <- lines <$> readFile "input"
    let ids = seatId . rowPos <$> values
    print $ mySeat ids
    
