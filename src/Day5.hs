module Main where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

walk :: (Int -> Int) -> [Int] -> IO Int
walk f xs = next 0 0 =<< V.thaw (V.fromList xs)
  where
    next steps i vect 
        | i < 0 || i >= M.length vect = return $! steps
        | otherwise = do
            d <- M.read vect i
            M.write vect i (f d)
            next (steps + 1) (i + d) vect

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
        res1 <- walk (+ 1) input
        putStrLn $ "Solution 1: " ++ show res1
        res2 <- walk (\i -> if i >= 3 then i - 1 else i + 1 ) input
        putStrLn $ "Solution 2: " ++ show res2