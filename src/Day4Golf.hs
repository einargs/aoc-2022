module Day4Golf (day4Golf) where

import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L

import Day

parsePair = (,) <$> (parseSection <* C.string ",") <*> parseSection <* C.space
  where parseSection = (,) <$> (L.decimal <* C.string "-") <*> L.decimal
inside (a, b) (c, d) = a >= c && b <= d
overlap (a, b) (c, d) = (c <= b && b <= d) || (a <= d && a >= c)
wrap f = T.pack . show . runParse p where
  g (s1, s2) = f s1 s2 || f s2 s1
  p = length . filter g <$> P.some (P.try parsePair)

day4Golf = answeredDay "4Golf" (wrap inside) (wrap overlap) "498" "859"
  `dayFile` "4"
