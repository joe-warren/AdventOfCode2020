import System.IO (readFile)
import Text.Parsec hiding (count, between)
import Control.Monad
import Text.Read (readMaybe)
import Data.Maybe (isJust)
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

between :: Int -> Int -> Int -> Bool
between lo hi v = v >= lo && v <= hi


parseNumber = read <$> many1 digit

parseHeight :: Parsec String () ()
parseHeight = do
    n <- parseNumber
    isCm <- (pure True <* string "cm") <|> (pure False <* string "in")
    if isCm
        then guard $ between 150 193 n
        else guard $ between 59 76 n

hexParser :: Parsec String () String 
hexParser = (:) <$> char '#' <*> sequence (replicate 6 (foldl (<|>) digit $ char <$> ['a'..'f']))

pidParser :: Parsec String () String
pidParser = sequence (replicate 9 digit)

parserToOption :: Parsec String () a -> String -> Maybe ()
parserToOption p s = case parse (p <* eof) "" s of
                        (Left _)  -> Nothing
                        (Right _) -> Just ()

checkEcl :: String -> Maybe ()
checkEcl s = if elem s ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"] then Just () else Nothing

validateRecord :: RawRecord -> Bool
validateRecord (RawRecord fields) = all check requiredFields
  where 
    check :: (String, String -> Maybe ()) -> Bool
    check (k, f) = isJust $ lookup k fields >>= f 
    requiredFields :: [(String, String -> Maybe ())]
    requiredFields = 
       [ ("byr", readMaybe >=> (guard . (between 1920 2002)))
       , ("iyr", readMaybe >=> (guard . (between 2010 2020))) 
       , ("eyr", readMaybe >=> (guard . (between 2020 2030)))
       , ("hgt", parserToOption parseHeight)
       , ("hcl", parserToOption hexParser)
       , ("ecl", checkEcl)
       , ("pid", parserToOption pidParser)
      ]

main :: IO ()
main = do 
    content <- readFile "input"
    case parseFile content of
        (Left err) -> print err
        (Right records) -> do 
            void $ traverse print records
            print $ count validateRecord records
