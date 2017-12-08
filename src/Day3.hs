module Main where

import Data.Monoid ((<>))

data Direction = U | L | D | R
data Position = P !Int !Int deriving (Eq, Show)

positionOf :: Int -> Position
positionOf p = positions !! (p - 1)

positions :: [Position]
positions = scanl (\p dir -> move dir p) (P 0 0) base
  where
    -- Build the direction list. [R, U, L, L, D, D, R, R, R, ...]
    base :: [Direction]
    base = concat [ replicate n d 
                  | (d, n) <- cycle [R, U, L, D] `zip` concatMap (\x -> [x,x]) [1..] ]

nextDirection :: Direction -> Direction
nextDirection U = L
nextDirection L = D
nextDirection D = R
nextDirection R = U

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
  where
    destination :: IO Int
    destination = readFile "input/Day3.txt" >>= return . read