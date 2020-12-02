import System.IO (readFile)
import Text.Parsec hiding (count)
import Control.Monad
data Record = Record 
    { low :: Int
    , high :: Int
    , character :: Char
    , password :: String
    } deriving Show

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f
   
parseFile :: String -> Either ParseError [Record]
parseFile s = parse recordParser "" s
  where
    recordParser = endBy line newline
    parseNumber = read <$> many1 digit
    line =  Record <$> parseNumber <* char '-' <*> parseNumber <* spaces <*> letter <* char ':' <* spaces <*> many letter 

validateRecord :: Record -> Bool
validateRecord r = let c = count ((==) $ character r) $ password r in
                    c >= low r && c <= high r

main :: IO ()
main = do 
    content <- readFile "input"
    case parseFile content of
        (Left err) -> print err
        (Right records) -> print $ count validateRecord records
