module Main where

import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

checksum :: [[Int]] -> Int
checksum input = sum $ [maximum d - minimum d | d <- input]

checksum2 :: [[Int]] -> Int
checksum2 input = sum $ [a `div` b 
                            | xs <- input
                            , a <- xs
                            , b <- xs
                            , a /= b && a `mod` b == 0]

parser :: Parser [[Int]]
parser = many1 (numbers <* newline) <* eof
  where
    numbers :: Parser [Int]
    numbers = (read <$> many1 digit) `sepBy1` (oneOf " \t")

main :: IO ()
main = do
    parseFromFile parser "input/Day2.txt" >>= \input ->
        case input of
            Left err -> print err
            Right arr -> do
                putStrLn $ "Solution 1: " ++ show (checksum arr)
                putStrLn $ "Solution 2: " ++ show (checksum2 arr)