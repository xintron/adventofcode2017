module Main where

import Data.List (sort)
import Data.Monoid ((<>))
import qualified Data.Set as S
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

isValid :: [String] -> Bool
isValid p = length p == length (S.fromList p)

parser :: Parser [[String]]
parser = many1 (phrases <* newline) <* eof
  where
    phrases :: Parser [String]
    phrases = many1 letter `sepBy1` (char ' ')

main :: IO ()
main = do
    parseFromFile parser "input/Day4.txt" >>= \passphrases ->
        case passphrases of
            Left err -> print err
            Right input -> do
                putStrLn $ "Solution 1: " <> show (validPassphrases input)
                putStrLn $ "Solution 2: " <> show (validPassphrases $ map (map sort) input)
  where
    validPassphrases :: [[String]] -> Int
    validPassphrases = length . filter (== True) . map isValid