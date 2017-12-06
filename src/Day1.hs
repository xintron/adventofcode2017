module Main where

import Data.Char

captcha :: (Eq a, Num a) => Int -> [a] -> a
captcha _ [] = 0
captcha offset xs = sum [x | (x, y) <- combine offset xs, x == y]
  where
    combine off digits = zip xs $ drop off $ digits ++ digits

main :: IO ()
main = do
    input <- readFile "input/Day1.txt" >>= mapM (return . digitToInt)
    print $ captcha 1 input
    print $ captcha (length input `div` 2) input