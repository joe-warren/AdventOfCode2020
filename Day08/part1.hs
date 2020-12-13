{-# language DeriveFunctor #-}
{-# language TupleSections #-}
import System.IO (readFile)
import Control.Monad
import Data.Functor
import Text.Parsec
import Text.Parsec.Number (int)

data Instruction = Nop | Jmp Int | Acc Int deriving Show

instructionParser :: Parsec String () [Instruction]
instructionParser = (`endBy` newline) $ nopParser <|> jmpParser <|> accParser
  where
    nopParser = (const Nop <$>) $ string "nop " *> int
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
        then Just acc
        else case fst . zipperFocus $ ins of 
            Nop -> ProgramState acc <$> (moveZipper 1 . setRead $ ins) >>= runProgram
            (Acc i) -> ProgramState (acc + i) <$> (moveZipper 1 . setRead $ ins) >>= runProgram
            (Jmp i) -> ProgramState acc <$> (moveZipper i . setRead $ ins) >>= runProgram
  where
    setRead = transformFocus ((, True) . fst)
 
runProgram' :: [Instruction] -> Maybe Int
runProgram' ins = (ProgramState 0 <$> zipperFromList ((, False) <$> ins)) >>= runProgram 

main :: IO ()
main = do 
    content <- readFile "input"
    case parseFile content of
        (Left err) -> print err
        (Right records) -> do 
            void $ traverse print records
            print $ runProgram' records
            
    
