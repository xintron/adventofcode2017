module Main where

import Control.Applicative ((<|>))
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe (mapMaybe, listToMaybe)
import qualified Data.Tree as T
import Text.Parsec hiding ((<|>))
import Text.Parsec.String (Parser, parseFromFile)
import Text.Parsec.Char (string)

type Program = (String, (Int, S.Set String))

findRoot :: [Program] -> String
findRoot ds = S.findMax $ S.fromList (map fst ds) `S.difference` children
  where
    children = S.unions $ map (snd . snd) ds

buildTree :: [Program] -> T.Tree Int
buildTree ds = T.unfoldTree treeFold $ findRoot ds
  where
    mem :: M.Map String (Int, S.Set String)
    mem = M.fromList ds
    treeFold :: String -> (Int, [String])
    treeFold seed = 
        let (weight, children) = mem M.! seed
         in (weight, S.toList children)

findBadDisc :: T.Tree Int -> Maybe Int
findBadDisc tr = listToMaybe children <|> isEven
  where
    children :: [Int]
    children = mapMaybe findBadDisc $ T.subForest tr
    isEven = case length $ S.fromList $ map sum $ T.subForest tr of
        x | x <= 1 -> Nothing
          | x == 2 -> Just (T.rootLabel tr)
        _ -> error "More than one anomaly found"

parser :: Parser [Program]
parser = values `sepEndBy` newline <* eof
  where
    values :: Parser Program
    values = (,) <$> many1 letter <*> ((,) <$> weight <*> children)
    weight :: Parser Int
    weight = spaces *> between (char '(') (char ')') (read <$> many1 digit)
    children :: Parser (S.Set String)
    children = S.fromList <$> option [] (string " -> " *> many1 letter `sepBy` string ", ")

withData :: Parser a -> IO a
withData p = do
    res <- parseFromFile p "input/Day7.txt"
    either (error . show) return res

main :: IO ()
main = do
    withData parser >>= \input -> do
        putStrLn $ "Solution 1: " ++ findRoot input
        return $ buildTree input
        putStrLn "Done"