import System.IO (readFile)
import Text.Parsec hiding (count)
import Control.Monad

newtype RawRecord = RawRecord [(String, String)] deriving Show

recordParser :: Parsec String () [RawRecord]
recordParser = (record `sepBy` (try $ newline <* newline)) <* newline <* eof
  where
    sepBy' p s = (:) <$> p <*> many (try (s *> p))
    record = RawRecord <$> sepBy' (field) (try (space <|> (newline)))
    field = (,) <$> (many1 alphaNum) <* char ':' <*> value
    value = many1 (alphaNum <|> char '#')

parseFile :: String -> Either ParseError [RawRecord]
parseFile s = parse recordParser "" s
  

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

validateRecord :: RawRecord -> Bool
validateRecord (RawRecord fields) = all ((flip elem) (fst <$> fields)) requiredFields
  where 
    requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

main :: IO ()
main = do 
    content <- readFile "input"
    case parseFile content of
        (Left err) -> print err
        (Right records) -> do 
            void $ traverse print records
            print $ count validateRecord records
