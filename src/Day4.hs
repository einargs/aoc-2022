module Day4 (day4) where

import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L

import Day

type Section = (Int, Int)

parsePair :: Parser (Section, Section)
parsePair = (,) <$> (parseSection <* C.string ",") <*> parseSection <* C.space
  where parseSection = (,) <$> (L.decimal <* C.string "-") <*> L.decimal

inside :: Section -> Section -> Bool
inside (a, b) (c, d) = a >= c && b <= d

overlap :: Section -> Section -> Bool
overlap (a, b) (c, d) = (c <= b && b <= d) || (a <= d && a >= c)

wrap :: (Section -> Section -> Bool) -> Text -> Text
wrap f = T.pack . show . runParse p where
  g (s1, s2) = f s1 s2 || f s2 s1
  p = length . filter g <$> P.some (P.try parsePair)

day4 :: Day
day4 = Day "4" (wrap inside) (wrap overlap) (Just "498") (Just "859")
