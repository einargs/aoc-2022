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
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as UV
import Control.Arrow (first, second)

import Day

type Forest = V.Vector (UV.Vector Int)
type Point = (Int, Int)

infixl 9 #

(#) :: Forest -> Point -> Int
v # (ui, bi) = (v V.! bi) UV.! ui

size :: Forest -> Point
size f = (UV.length $ f V.! 0, V.length f)

parseForest :: Text -> Forest
parseForest = V.fromList . fmap buildRow . T.splitOn "\n" . T.strip where
  f c = fromEnum c - fromEnum '0'
  buildRow = UV.fromList . fmap f . T.unpack

indices :: Forest -> [Point]
indices f = [0..x-1] >>= g where
  (x, y) = size f
  g i = (i,) <$> [0..y-1]

sightline :: Forest -> Point -> (Point -> Point) -> [Int]
sightline f s inc = g [] $ inc s where
  (mx, my) = size f
  bounded (x,y) = x < mx && 0 <= x && y < my && 0 <= y
  g acc p | bounded p = g (f # p:acc) (inc p)
          | otherwise = acc

visible :: Forest -> (Int, Int) -> Bool
visible f p = check up || check down || check left || check right where
  height = f # p
  check g = all (<height) $ sightline f p g
  up = second (subtract 1)
  down = second (+1)
  left = first (subtract 1)
  right = first (+1)

part1 :: Forest -> Int
part1 f = length $ filter (visible f) $ indices f

part2 :: Forest -> Int
part2 = undefined

wrap :: (Forest -> Int) -> Text -> Text
wrap f = T.pack . show . f . parseForest

day8 :: Day
day8 = mkDay "8" (wrap part1) (wrap part2)
