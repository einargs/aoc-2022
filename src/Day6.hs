module Day6 (day6) where

import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec ((<|>))
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as C
import Data.Sequence (Seq((:<|)))
import Data.Sequence qualified as S
import Data.Functor (($>))
import Data.List qualified as L
import Data.Foldable qualified as F

import Day

solve :: Int -> String -> Int
solve len = f 0 where
  f i s | isCorrect s = i + len
        | otherwise = f (i+1) $ tail s
  isCorrect ls = len == length (L.nub $ take len ls)

wrap :: (String -> Int) -> Text -> Text
wrap p = T.pack . show . p . T.unpack

day6 :: Day
day6 = mkDay "6" (wrap part1) (wrap part2) where
  part1 = solve 4
  part2 = solve 14
