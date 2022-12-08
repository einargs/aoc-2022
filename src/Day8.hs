module Day8 (day8) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector.Unboxed qualified as UV
import Control.Arrow (first, second)

import Day

type Forest = (Int, UV.Vector Int)
type Point = (Int, Int)

infixl 9 #

(#) :: Forest -> Point -> Int
(x, uv) # (xi, yi) = uv UV.! ((x*yi) + xi)

size :: Forest -> Point
size (x, uv) = (x, UV.length uv `div` x)

parseForest :: Text -> Forest
parseForest t = (T.length $ head ts, UV.fromList $ buildRow =<< ts) where
  ts = T.splitOn "\n" $ T.strip t
  f c = fromEnum c - fromEnum '0'
  buildRow = fmap f . T.unpack

indices :: Forest -> [Point]
indices f = [0..x-1] >>= g where
  (x, y) = size f
  g i = (i,) <$> [0..y-1]

sightline :: Forest -> Point -> (Point -> Point) -> [Int]
sightline f s inc = reverse $ g [] $ inc s where
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

scenicScore :: Forest -> Point -> Int
scenicScore f p = score up * score down * score left * score right where
  height = f # p
  metric i [] = i
  metric i (x:xs) | x<height = metric (i+1) xs
                  | otherwise = i+1
  score g = metric 0 $ sightline f p g
  up = second (subtract 1)
  down = second (+1)
  left = first (subtract 1)
  right = first (+1)

part2 :: Forest -> Int
part2 f = maximum $ scenicScore f <$> indices f

wrap :: (Forest -> Int) -> Text -> Text
wrap f = T.pack . show . f . parseForest

day8 :: Day
day8 = answeredDay "8" (wrap part1) (wrap part2) "1779" "172224"
