module Day9 (day9) where

import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec ((<|>))
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L
import Data.Functor (($>))
import Control.Arrow (first, second)
import Data.List (nub, scanl', foldl')
import Debug.Trace qualified as DT

import Day

data Dir = R | L | U | D
type Move = (Dir, Int)

parseMove :: Parser Move
parseMove = (,) <$> (dir <* C.space) <*> L.decimal <* C.newline where
  d s p = C.string s $> p
  dir = d "R" R <|> d "L" L <|> d "U" U <|> d "D" D

parseInput :: Parser [Move]
parseInput = P.some parseMove <* P.eof

type Pos = (Int, Int)

advanceHead :: Dir -> Pos -> Pos
advanceHead R = first (+1)
advanceHead L = first $ subtract 1
advanceHead U = second (+1)
advanceHead D = second $ subtract 1

updateTail :: Pos -> Pos -> Pos
updateTail (hx, hy) (tx, ty)
  | adjacent = (tx, ty)
  | straight hx tx hy ty = (tx `incBy` hx, ty)
  | straight hy ty hx tx = (tx, ty `incBy` hy)
  | diag hy ty hx tx = (tx `incBy` hx, hy)
  | diag hx tx hy ty = (hx, ty `incBy` hy)
  | ediag hx tx hy ty = (tx `incBy` hx, ty `incBy` hy)
  | otherwise = error $ "shouldn't happen " <> show ((hx,hy),(tx,ty))
  where
    incBy tv hv = tv + ((hv-tv) `div` 2)
    adjacent = abs (hx-tx) <= 1 && abs (hy-ty) <= 1
    straight x1 x2 y1 y2 = abs (x1-x2) == 2 && abs (y1-y2) == 0
    diag x1 x2 y1 y2 = abs (x1-x2) == 1 && abs (y1-y2) == 2
    ediag x1 x2 y1 y2 = abs (x1-x2) == 2 && abs (y1-y2) == 2

part1 :: [Move] -> Int
part1 ms = length (nub $ sim ((0,0),(0,0)) ms []) where
  advance :: Dir -> (Pos, Pos) -> (Pos, Pos)
  advance d (t, h) = (updateTail h' t, h') where
    h' = advanceHead d h
  sim :: (Pos, Pos) -> [Move] -> [Pos] -> [Pos]
  sim _ [] ts = ts
  sim st ((_, 0):ms) ts = sim st ms ts
  sim st ((d, i):ms) ts = sim st' ((d, i-1):ms) (t':ts)
    where st'@(t', _) = advance d st

part2 :: [Move] -> Int
part2 ms = length (nub $ sim (replicate 9 (0,0), (0,0)) ms []) where
  -- The tail is stored so the end of the tail is the last
  advance :: Dir -> ([Pos], Pos) -> ([Pos], Pos)
  advance d (ts, h) = (ts', h') where
    h' = advanceHead d h
    f (lh, xs) lt = (lt', lt':xs) where lt' = updateTail lh lt
    ts' = reverse $ snd $ foldl' f (h',[]) ts 
  sim :: ([Pos], Pos) -> [Move] -> [Pos] -> [Pos]
  sim _ [] fs = fs
  sim st ((_, 0):ms) fs = sim st ms fs
  sim st ((d, i):ms) fs = sim st' ((d, i-1):ms) ((ts' !! 8):fs)
    where st'@(ts', _) = advance d st

wrap :: ([Move] -> Int) -> Text -> Text
wrap f = T.pack . show . f . runParse parseInput

day9 :: Day
day9 = answeredDay "9" (wrap part1) (wrap part2) "6018" "2619"


