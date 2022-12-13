module Day13 (day13) where

import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec ((<|>))
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L
import Data.Maybe (fromJust)
import Data.List (elemIndex, sort)

import Day

data Packet = Val Int | List [Packet] deriving (Show, Eq)

parsePacket :: Parser Packet
parsePacket = val <|> list where
  val = Val <$> L.decimal
  list = P.between (C.char '[') (C.char ']') $
    List <$> parsePacket `P.sepBy` C.char ','

instance Ord Packet where
  compare p1@(Val _) p2@(List _) = compare (List [p1]) p2
  compare p1@(List _) p2@(Val _) = compare p1 (List [p2])
  compare (Val a) (Val b) = compare a b
  compare (List a) (List b) = compare a b

part1 :: Parser Int
part1 = solve <$> parseInput where
  parseInput :: Parser [(Packet, Packet)]
  parseInput = pair `P.sepBy` P.try (C.newline *> C.newline) where
    pair = (,) <$> (parsePacket <* C.newline) <*> parsePacket
  solve :: [(Packet, Packet)] -> Int
  solve = sum . fmap fst . filter (uncurry (<) . snd) . zip [1..]

part2 :: Parser Int
part2 = fmap solve $ P.some $ parsePacket <* P.some C.newline where
  solve :: [Packet] -> Int
  solve ps = f 2 * f 6 where
    divp n = List [List [Val n]]
    ps' = sort $ divp 2:divp 6:ps
    f n = 1 + fromJust (elemIndex (divp n) ps')

wrap :: Parser Int -> Text -> Text
wrap p = T.pack . show . runParse p

day13 :: Day
day13 = answeredDay "13" (wrap part1) (wrap part2) "5760" "26670"
