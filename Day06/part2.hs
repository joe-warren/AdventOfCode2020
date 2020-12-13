import System.IO (readFile)
import Data.List.Split (splitOn)
import Data.List (intersect)
import Control.Monad

groups :: [String] -> [String]
groups s = foldl intersect ['a'..'z'] <$> g'
  where 
    g' = splitOn [""] s
      

main :: IO ()
main = do 
    values <- lines <$> readFile "input"
    let g = groups values
    void $ traverse print g
    print $ sum $ length <$> g
    
