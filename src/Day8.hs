module Day8 (day8) where

import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec ((<|>))
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as C
import Data.List qualified as L
import Data.Functor (($>))
import Control.Monad (void, (<=<))
import Debug.Trace qualified as DT

import Day

type Forest = [[Int]]

parseForest :: Text -> Forest
parseForest = fmap (fmap f . T.unpack) . T.splitOn "\n" . T.strip where
  f c = fromEnum c - fromEnum '0'

sightDepth [] = error "should not happen"
sightDepth (t:ts) = f (1,[0]) t ts where
  f (i, acc) _ [] = acc
  f (i, acc) m (c:xs) | m < c = f (i+1, i:acc) c xs
                      | otherwise = f (i+1, acc) m xs

rowIndices :: [Int] -> [Int]
rowIndices row = sightDepth row <> fmap (l-) (sightDepth row) where
  l = length row - 1
allRows :: Forest -> [(Int, Int)]
allRows = snd <=< L.scanl' f (0, []) where
  f (i, _) row = (i+1, DT.traceShowId $ (,i) <$> rowIndices row)
part1 :: Forest -> Int
part1 forest = length $ DT.traceShowId $ L.nub $ visibleTrees where
  visibleTrees = allRows forest <> top where
    top = f <$> allRows (L.transpose forest)
    f (a, b) = (b, a)

part2 :: Forest -> Int
part2 = undefined

wrap :: (Forest -> Int) -> Text -> Text
wrap f = T.pack . show . f . parseForest

day8 :: Day
day8 = mkDay "8" (wrap part1) (wrap part2)
