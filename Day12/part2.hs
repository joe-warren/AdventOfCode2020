import System.IO (readFile)
import Text.Parsec 
import Control.Monad

data Compass = N | E | S | W deriving (Show, Eq, Enum)

data Instruction = Direction Compass Int | Rotation Int | Forward Int deriving (Show, Eq)

recordParser = record `endBy` newline 
  where
    int = read <$> many1 digit
    directionParser dir c = Direction dir <$> (char c *> int)
    n = directionParser N 'N'
    s = directionParser S 'S'
    e = directionParser E 'E'
    w = directionParser W 'W'
    l = Rotation . (4 - ) . (`div` 90) <$> (char 'L' *> int)
    r = Rotation . (`div` 90) <$> (char 'R' *> int)
    f = Forward <$> (char 'F' *> int)
    record = choice [n, s, e, w, l, r, f]

data BoatState = BoatState 
        { boatPos :: (Int, Int)
        , waypointPos :: (Int, Int)
        } deriving (Eq, Show)

moveDirection :: Compass -> Int -> (Int, Int) -> (Int, Int)
moveDirection N d (x, y) = (x, y+d) 
moveDirection E d (x, y) = (x+d, y) 
moveDirection S d (x, y) = (x, y-d) 
moveDirection W d (x, y) = (x-d, y) 

rotatePoint :: Int -> (Int, Int) -> (Int, Int)
rotatePoint 1 (x, y) = (y, -x)
rotatePoint 2 (x, y) = (-x, -y)
rotatePoint 3 (x, y) = (-y, x)
rotatePoint r (x, y) = error $ "didn't expect to rotate by " ++ show r


executeInstruction :: Instruction -> BoatState -> BoatState
executeInstruction (Direction d v) (BoatState p wp) = BoatState p (moveDirection d v wp)
executeInstruction (Forward v) (BoatState (x, y) wp@(dx, dy)) = BoatState (x + dx*v, y + dy*v) wp
executeInstruction (Rotation r) (BoatState p wp)  = BoatState p (rotatePoint r wp)

parseFile :: String -> Either ParseError [Instruction]
parseFile s = parse recordParser "" s

main :: IO ()
main = do 
    content <- readFile "input"
    case parseFile content of
        (Left err) -> print err
        (Right records) -> do 
            void $ traverse print records
            let res = foldl (flip executeInstruction) (BoatState (0, 0) (10, 1)) records
            let (x, y) = boatPos res
            let r = abs x + abs y 
            print res
            print r
