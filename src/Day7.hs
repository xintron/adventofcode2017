module Main where

import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)
import Text.Parsec.Char (string)

type Input = (String, (Int, Maybe [String]))

parser :: Parser [Input]
parser = values `sepEndBy` newline <* eof
  where
    values = do
        k <- many1 letter
        w <- weight
        c <- optionMaybe children
        return (k, (w, c))
    weight :: Parser Int
    weight = spaces *> between (char '(') (char ')') (read <$> many1 digit)
    children :: Parser [String]
    children = string " -> " *> many1 letter `sepBy` string ", "

findRoot :: [Input] -> String
findRoot ds = S.findMax $ S.fromList (map fst ds) `S.difference` children
  where
    children = S.unions $ map (S.fromList . fromMaybe [] . snd . snd) ds

withData :: FilePath -> Parser a -> IO a
withData file p = do
    res <- parseFromFile p file
    either (error . show) return res

main :: IO ()
main = do
    withData "input/Day7.txt" parser >>= \input -> do
        putStrLn $ "Solution 1: " ++ findRoot input