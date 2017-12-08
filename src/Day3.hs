module Main where

import Data.List (mapAccumL)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))

data Direction = U | L | D | R
data Position = P !Int !Int deriving (Ord, Eq, Show)

positionOf :: Int -> Position
positionOf p = positions !! (p - 1)

positions :: [Position]
positions = scanl (\p dir -> move dir p) (P 0 0) base
  where
    -- Build the direction list. [R, U, L, L, D, D, R, R, R, ...]
    base :: [Direction]
    base = concat [ replicate n d 
                  | (d, n) <- cycle [R, U, L, D] `zip` concatMap (\x -> [x,x]) [1..] ]

-- Starting at the center, take the sum of the connected neighbours for any previous position in the positions-spiral
-- Should create a list matching [1, 1, 2, 4, 5, 10, ...]
sumPositions :: [Int]
sumPositions = snd $ mapAccumL goNext (Map.singleton (P 0 0) 1) positions
  where
    goNext :: Map.Map Position Int -> Position -> (Map.Map Position Int, Int)
    goNext m p = (Map.insert p currentSum m, currentSum)
      where
        currentSum = sum $ mapMaybe (`Map.lookup` m) (neighbours p)

-- Generate neighbours
neighbours :: Position -> [Position]
neighbours (P cx cy) = [P (cx + x) (cy + y)
                       | x <- [-1..1]
                       , y <- [-1..1]
                       ]

move :: Direction -> Position -> Position
move U (P x y) = P x (y + 1)
move L (P x y) = P (x - 1) y
move D (P x y) = P x (y - 1)
move R (P x y) = P (x + 1) y

main :: IO ()
main = do
    input <- destination
    let (P x y) = positionOf input
    putStrLn $ "Length to position " <> show input <> ": " <> show (abs x + abs y)
    putStrLn $ "Part 2: " <> show (head $ dropWhile (<= input) sumPositions)
  where
    destination :: IO Int
    destination = readFile "input/Day3.txt" >>= return . read