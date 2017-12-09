module Main where

import qualified Data.Sequence as S
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

walk :: (Int -> Int) -> [Int] -> Int
walk f xs = next 0 0 (S.fromList xs)
  where
    next steps i seq
        | i < 0 || i >= S.length seq = steps
        | otherwise =
            let d = S.index seq i
             in next (steps + 1) (i + d) (S.update i (f d) seq)

parser :: Parser [Int]
parser = many1 (numbers <* newline) <* eof
  where
    numbers :: Parser Int
    numbers = do
        negative <- option ' ' (char '-')
        d <- many1 digit
        return $ read (negative : d)

withData :: FilePath -> Parser a -> IO a
withData file p = do
    res <- parseFromFile p file
    either (error . show) return res

main :: IO ()
main = do
    withData "input/Day5.txt" parser >>= \input -> do
        putStrLn $ "Solution 1: " ++ show (walk (+ 1) input)
        putStrLn $ "Solution 2: " ++ show (walk (\i -> if i >= 3 then i - 1 else i + 1 ) input)