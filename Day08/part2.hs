{-# language DeriveFunctor #-}
{-# language TupleSections #-}
import System.IO (readFile)
import Control.Monad
import Data.Functor
import Data.Maybe (catMaybes)
import Text.Parsec hiding ((<|>))
import Control.Applicative ((<|>))
import Text.Parsec.Number (int)

data Instruction = Nop Int | Jmp Int | Acc Int deriving Show

instructionParser :: Parsec String () [Instruction]
instructionParser = (`endBy` newline) $ nopParser <|> jmpParser <|> accParser
  where
    nopParser = (Nop <$>) $ string "nop " *> int
    jmpParser = (Jmp <$>) $ string "jmp " *> int
    accParser = (Acc <$>) $ string "acc " *> int

parseFile :: String -> Either ParseError [Instruction]
parseFile = parse instructionParser ""

data ListZipper a = ListZipper
        { zipperFocus :: a
        , zipperLeft :: [a]
        , zipperRight :: [a]
        } deriving (Show, Eq, Functor)

zipperFromList :: [a] -> Maybe (ListZipper a)
zipperFromList (h:t) = Just $ ListZipper h [] t
zipperFromList [] = Nothing

safeHead :: [a] -> Maybe a
safeHead (h:_) = Just $ h
safeHead [] = Nothing

moveZipper :: Int -> ListZipper a -> Maybe (ListZipper a)
moveZipper 0 x = Just x
moveZipper i x | i > 0 = moveRight i x
               | otherwise = moveLeft (abs i) x 
    where
        moveRight j (ListZipper f l r) = ListZipper <$>
            (safeHead $ drop (j-1) r) <*> 
            (pure $ (reverse $ take (j-1) r) <> [f] <> l) <*> 
            (pure $ drop j r)
        moveLeft j (ListZipper f l r) = ListZipper <$>
            (safeHead $ drop (j-1) l) <*>
            (pure $ drop j l) <*>
            (pure $ (reverse $ take (j-1) l) <> [f] <> r)

transformFocus :: (a -> a) -> ListZipper a -> ListZipper a
transformFocus fn (ListZipper f l r) = ListZipper (fn f) l r

data ProgramState = ProgramState 
                        { programAccumulator :: Int
                        , programInstructions :: ListZipper (Instruction, Bool)
                        } deriving Show

runProgram :: ProgramState -> Maybe Int
runProgram (ProgramState acc ins) = 
    if snd . zipperFocus $ ins 
        then Nothing -- loop
        else case fst . zipperFocus $ ins of 
            Nop _ -> finished 1 0 <|> (ProgramState acc <$> (moveZipper 1 . setRead $ ins) >>= runProgram)
            (Acc i) -> finished 1 i <|> (ProgramState (acc + i) <$> (moveZipper 1 . setRead $ ins) >>= runProgram)
            (Jmp i) -> finished i 0 <|> (ProgramState acc <$> (moveZipper i . setRead $ ins) >>= runProgram)
  where
    setRead = transformFocus ((, True) . fst)
    finished i j  = if i == 1 + (length . zipperRight $ ins)
                    then Just (acc + j)
                    else Nothing 
 
runProgram' :: [Instruction] -> Maybe Int
runProgram' ins = (ProgramState 0 <$> zipperFromList ((, False) <$> ins)) >>= runProgram 

runAllPrograms :: [Instruction] -> Maybe Int
runAllPrograms ins = safeHead . catMaybes $  runProgram' <$> possibleInstructions ins

possibleInstructions :: [Instruction] -> [[Instruction]]
possibleInstructions ((Nop i):t) = ((Jmp i):t):(((Nop i):) <$> possibleInstructions t)
possibleInstructions ((Jmp i):t) = ((Nop i):t):(((Jmp i):) <$> possibleInstructions t)
possibleInstructions ((Acc i):t) = (((Acc i):) <$> possibleInstructions t)
possibleInstructions [] = [[]]


main :: IO ()
main = do 
    content <- readFile "input"
    case parseFile content of
        (Left err) -> print err
        (Right records) -> do 
            void $ traverse print records
            print $ runAllPrograms records
            
    
