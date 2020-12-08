import System.IO (readFile)
import Text.Parsec hiding (count)
import Control.Monad
import Data.Functor
import Data.List (nub)
import Data.Maybe (fromMaybe)

data RawRecord = RawRecord 
                    { recordName :: String
                    , recordChildren :: [(Int, String)]
                    } deriving (Show, Eq)

bagNameParser :: Parsec String () String
bagNameParser = (many letter) <> (pure <$> space) <> (many letter)

recordParser :: Parsec String () [RawRecord]
recordParser = (`endBy` newline) $ do
    name <- bagNameParser
    _ <- string " bags contain "
    children <- ((string "no other bags" $> []) <|>) $ (`sepBy` (string ", ")) $ do
        n <- read <$> many1 digit
        _ <- space
        child <- bagNameParser
        _ <- (void $ string " bag") <> (optional $ char 's')
        return (n, child)
    _ <- char '.'
    return $ RawRecord name children

parseFile :: String -> Either ParseError [RawRecord]
parseFile s = parse recordParser "" s
  
parents :: [RawRecord] -> String -> [RawRecord]
parents records k = nub $ theseParents <> grandparents
    where 
        theseParents = filter (any ((== k).snd) . recordChildren) records
        grandparents = (recordName <$> theseParents) >>= (parents records) 


children :: [RawRecord] -> String -> Int
children records k = 1 + nestedBags
  where
    assocList :: [(String, [(Int, String)])]
    assocList = (\r -> (recordName r, recordChildren r)) <$> records
    theseChildren = fromMaybe [] (lookup k assocList)
    nestedBags = sum $ (\(n, c) -> (children records c) * n) <$> theseChildren
        

main :: IO ()
main = do 
    content <- readFile "input"
    case parseFile content of
        (Left err) -> print err
        (Right records) -> do 
            void $ traverse print records
            print $ (children records "shiny gold") - 1
            
