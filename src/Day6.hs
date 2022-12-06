module Day6 (day6) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.List qualified as L

import Day

solve :: Int -> Text -> Text
solve len = T.pack . show . f 0 . T.unpack where
  f i s | isCorrect s = i + len
        | otherwise = f (i+1) $ tail s
  isCorrect ls = len == length (L.nub $ take len ls)

day6 :: Day
day6 = answeredDay "6" (solve 4) (solve 14) "1042" "2980"
