module Main where

import Data.Monoid ((<>))
import qualified Data.Set as S

data Direction = U | L | D | R
type Position = (Int, Int)

data Grid = Grid
    { current :: Position
    , direction :: Direction
    , seen :: S.Set Position
    }

-- Make sure to use the Down initial position. This will make sure the first one
-- testes is the position to the Right
positionOf :: Int -> Position
positionOf p = current $ (iterate next $ Grid (0, 0) D (S.singleton (0, 0)) ) !! (p - 1)
  where
    next :: Grid -> Grid
    next g = if current (moveNext g) `S.notMember` seen g
        -- Update the direction and move. If not seen store it and move on
        then let mn = moveNext g
              in mn { seen = S.insert (current mn) (seen g) }
        -- Can't turn and move. Move without changing direction. Add to seen
        else let mp = move (direction g) (current g)
              in g { current = mp, seen = S.insert mp (seen g) }
    moveNext :: Grid -> Grid
    moveNext gg = let newPosition = move (nextDirection $ direction gg) $ current gg
                   in gg { current = newPosition, direction = nextDirection $ direction gg }

nextDirection :: Direction -> Direction
nextDirection U = L
nextDirection L = D
nextDirection D = R
nextDirection R = U

move :: Direction -> Position -> Position
move U (x, y) = (x, y + 1)
move L (x, y) = (x - 1, y)
move D (x, y) = (x, y - 1)
move R (x, y) = (x + 1, y)

main :: IO ()
main = do
    input <- destination
    let (x, y) = positionOf input
    putStrLn $ "Length to position " <> show input <> ": " <> show (abs x + abs y)
  where
    destination :: IO Int
    destination = readFile "input/Day3.txt" >>= return . read