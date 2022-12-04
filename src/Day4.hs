module Day4 (day4) where

import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L
import Data.Char (ord)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Map (Map)
import Data.Map qualified as M
import Data.Functor (($>))

import Day

parseContaining :: Parser Bool
parseContaining = do
  s1 <- parseSection
  C.string ","
  s2 <- parseSection
  C.space
  pure $ inside s1 s2 || inside s2 s1
  where
    parseSection = (,) <$> (L.decimal <* C.string "-") <*> L.decimal
    inside (a, b) (c, d) = a >= c && b <= d


part1 :: Parser Int
part1 = (length . filter id) <$> P.some (P.try parseContaining)

part2 :: Parser Int
part2 = do
  undefined

wrap :: Show a => Parser a -> Text -> Text
wrap p = T.pack . show . runParse p

day4 :: Day
day4 = Day "4" (wrap part1) (wrap part2) Nothing Nothing
