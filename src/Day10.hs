module Day10 (day10) where

import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec ((<|>))
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L
import Data.Functor (($>))
import Control.Arrow (first, second)
import Data.List (splitAt)
import Debug.Trace qualified as DT

import Day

data Inst = NoOp | Add Int

parseInst :: Parser Inst
parseInst = (C.string "noop" $> NoOp) <|> add where
  add = C.string "addx"  *> C.space *> (Add <$> sign)
  sign = L.signed (pure ()) L.decimal

parseInput :: Parser [Inst]
parseInput = P.some $ parseInst <* C.newline

data State = State {cpuCycle :: Int, cpuRegister :: Int, cpuOut :: [Int]}
  deriving Show

part1 :: [Inst] -> Int
part1 insts = sum $ take 6 $ reverse $ f (State 0 1 []) insts where
  f :: State -> [Inst] -> [Int]
  f (State _ _ out) [] = out
  f s (i:is) = f (exec s i) is
  interesting i = (i+20) `mod` 40 == 0
  pause :: State -> Int -> State
  pause s 0 = s
  pause (State c r o) i = pause (State c' r o') (i-1) where
    c' = c + 1
    o' = if interesting c' then (c'*r):o else o
  exec :: State -> Inst -> State
  exec s NoOp = pause s 1
  exec s (Add n) = State c (r+n) o where (State c r o) = pause s 2

data Crt = Crt {crtCycle :: Int, crtRegister :: Int, crtOut :: [Bool]}

part2 :: [Inst] -> String
part2 insts = format $ fmap toC $ reverse $ f (Crt 0 1 []) insts where
  toC True = '#'
  toC False = '.'
  f :: Crt -> [Inst] -> [Bool]
  f (Crt _ _ out) [] = out
  f s (i:is) = f (exec s i) is
  pause :: Crt -> Int -> Crt
  pause s 0 = s
  pause (Crt c r o) i = pause (Crt c' r o') (i-1) where
    c' = c + 1
    o' = (abs ((c `mod` 40) - r) <= 1):o
  exec :: Crt -> Inst -> Crt
  exec s NoOp = pause s 1
  exec s (Add n) = Crt c (r+n) o where (Crt c r o) = pause s 2
  format :: String -> String
  format s = if s' /= [] then l <> "\n" <> format s' else l <> "\n" where
    (l, s') = splitAt 40 s

wrap :: ([Inst] -> a) -> Text -> a
wrap f = f . runParse parseInput

-- part1 is 12560, part2 is PLPAFBCL
day10 :: Day
day10 = mkDay "10" (T.pack . show . wrap part1) (T.pack . wrap part2)
