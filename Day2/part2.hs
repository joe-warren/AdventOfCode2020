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

at :: Int -> [a] -> Maybe a
at 0 (x:_) = Just x
at n (x:xs) = at (n-1) xs
at _ [] = Nothing

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

parseFile :: String -> Either ParseError [Record]
parseFile s = parse recordParser "" s
  where
    recordParser = endBy line newline
    parseNumber = read <$> many1 digit
    line =  Record <$> parseNumber <* char '-' <*> parseNumber <* spaces <*> letter <* char ':' <* spaces <*> many letter 

validateRecord :: Record -> Bool
validateRecord r = 
    let v1 = at (low r - 1) (password r) in
    let v2 = at (high r - 1) (password r) in
    (v1 == Just (character r)) `xor` (v2 == Just (character r))

main :: IO ()
main = do 
    content <- readFile "input"
    case parseFile content of
        (Left err) -> print err
        (Right records) -> print $ count validateRecord records
