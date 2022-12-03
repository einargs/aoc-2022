module Day2Golf (day2Golf) where

import Data.Text qualified as T
import Data.Char (ord)
import Day

part1 = sum . fmap f where
  f (a, b) | a == b = b + 4
           | b == ((a + 1) `mod` 3) = b + 7
           | otherwise = b + 1

part2 = sum . fmap f where
  f (a, b) = b*3 + ((a + (b - 1)) `mod` 3) + 1

wrap f = T.pack . show . f . fmap parse . T.splitOn "\n" . T.init where
  parse t = (ord (T.head t) - ord 'A', ord (T.last t) - ord 'X')

day2Golf = Day "2Golf" (wrap part1) (wrap part2) (Just "15422") (Just "15442")
