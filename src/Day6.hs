module Main where

import qualified Data.Map as M
import qualified Data.Vector as V
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

cycles :: [Int] -> [V.Vector Int]
cycles xs = iterate next (V.fromList xs)
   where
     next :: V.Vector Int -> V.Vector Int
     next ds = nv
      where
        maxInt = V.maxIndex ds
        nv = redistribute maxInt (ds V.! maxInt) (ds V.// [(maxInt, 0)])

-- Redistribute the set starting at index `i`, adding 1 to all positions
-- wrapping around the set
redistribute :: Int -> Int -> V.Vector Int -> V.Vector Int
redistribute index amount ds = V.accum (+) ds [(x `mod` length ds, 1)
                                              | x <- [index+1 .. index+amount]
                                              ]

run :: [Int] -> (Int, Int)
run ds = go 0 M.empty $ cycles ds
  where
    go n mem (x:xs) = if M.member x mem
        then (n, n - mem M.! x)
        else go (n + 1) (M.insert x n mem) xs

parser :: Parser [Int]
parser = (numbers `sepBy1` oneOf " \t") <* eof
  where
    numbers :: Parser Int
    numbers = read <$> many1 digit

withData :: FilePath -> Parser a -> IO a
withData file p = do
    res <- parseFromFile p file
    either (error . show) return res

main :: IO ()
main = do
    withData "input/Day6.txt" parser >>= \input -> do
        let res = run input
        putStrLn $ "Solution 1: " ++ show (fst res)
        putStrLn $ "Solution 2: " ++ show (snd res)