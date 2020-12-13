{-# LaNguAgE RankNTypes #-}
import Control.Monad
import Control.Lens
import System.IO (readFile)
import Data.Vector (Vector)
import Data.Maybe (fromMaybe) 
import qualified Data.Vector as V
import Data.Monoid (Sum (..))

data Space = NoSeat | Seat Bool deriving (Show, Eq)

parseChar :: Char -> Maybe Space
parseChar '.' = Just NoSeat
parseChar '#' = Just $ Seat True
parseChar 'L' = Just $ Seat False
parseChar _ = Nothing

printSpace :: Space -> Char
printSpace NoSeat = '.'
printSpace (Seat True) = '#'
printSpace (Seat False) = 'L'

type Grid a = Vector (Vector a)

printGrid :: Grid Space -> String
printGrid = unlines . V.toList . (fmap (V.toList . (fmap printSpace)))

gridIndex :: (Int, Int) -> Traversal' (Grid a) a
gridIndex i = ix (fst i) . ix (snd i)

neighbours :: [(Int, Int) -> (Int, Int)]
neighbours = [\(x, y) -> (x + dx, y+dy) | dx <- [-1..1], dy<- [-1..1], (dx,dy) /= (0, 0)]

transformSpace :: Space -> Int -> Space
transformSpace NoSeat _ = NoSeat
transformSpace (Seat False) 0 = Seat True  
transformSpace (Seat False) _ = Seat False
transformSpace (Seat True) i | i < 5 = Seat True  
                             | otherwise = Seat False


lookupSeat :: Grid Space -> (Int, Int)-> ((Int, Int) -> (Int, Int)) -> Space
lookupSeat g i f = let n = f i in
    case g ^? gridIndex n of
        Nothing -> NoSeat
        Just NoSeat -> lookupSeat g n f
        Just s@(Seat _) -> s

stepSpace :: (Int, Int) -> Space -> Grid Space -> Space
stepSpace i s g = transformSpace s n
  where
    n = length $ filter (== Seat True) $ (lookupSeat g i <$> neighbours)

step :: Grid Space -> Grid Space
step g = itraverseOf (itraversed <.> itraversed) stepSpace g g

runPart1 :: Grid Space -> Grid Space 
runPart1 g | next == g = next
           | otherwise = runPart1 next
  where 
    next = step g 

countOccupied :: Grid Space -> Int
countOccupied g = getSum $ mconcat $ fn <$> g ^.. traversed . traversed 
  where 
    fn (Seat True) = Sum 1
    fn _ = Sum 0

main :: IO ()
main = do
    l <- lines <$> readFile "input"
    case (traverse.traverse) parseChar l of
      Nothing -> putStrLn "invalid data"
      (Just seatsL) -> do
        let seats = V.fromList <$> V.fromList seatsL
        let sln = runPart1 seats
        putStrLn $ printGrid sln   
        print $ countOccupied sln
