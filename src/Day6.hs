module Main where

import qualified Data.Set as Set
import qualified Data.Vector as V
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

cycles :: [Int] -> Int
cycles xs = next 1 (V.fromList xs) (Set.singleton $ V.fromList xs)
   where
     next :: Int -> V.Vector Int -> Set.Set (V.Vector Int) -> Int
     next i ds mem = case V.elemIndex (V.maximum ds) ds of
        Nothing -> error "No index found"
        Just pos -> do
            -- Update the list and then redistribute it
            let ns = redistribute pos (maximum ds) $ ds V.// [(pos, 0)]
            if Set.member ns mem
                then i
                else next (i + 1) ns (Set.insert ns mem)

-- Redistribute the set starting at index `i`, adding 1 to all positions
-- wrapping around the set
redistribute :: Int -> Int -> V.Vector Int -> V.Vector Int
redistribute index amount ds = V.accum (+) ds [(x `mod` length ds, 1)
                                              | x <- [index+1 .. index+amount]
                                              ]

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
        putStrLn $ "Solution 1: " ++ show (cycles input)