module Day2 (day2) where

import qualified Data.Text as T
import qualified Text.Megaparsec as P
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Functor (($>))

import Day

data RPS = Rock | Paper | Scissors deriving (Eq, Show)

type Round = (RPS, RPS)

parseRound :: Parser Round
parseRound = (,) <$> rpsABC <*> rpsXYZ where
  mkRps a b c = (C.string a $> Rock
    <|> C.string b $> Paper
    <|> C.string c $> Scissors) <* C.space
  rpsABC = mkRps "A" "B" "C"
  rpsXYZ = mkRps "X" "Y" "Z"

score :: Round -> Int
score (a, b) = play b + win a b where
  play Rock = 1
  play Paper = 2
  play Scissors = 3
  inv 0 = 6
  inv 6 = 0
  inv x = x
  win Rock Scissors = 0
  win Rock Paper = 6
  win Scissors Paper = 0
  win x y | x == y = 3
          | otherwise = inv $ win y x

part1 :: Parser Int
part1 = sum <$> P.some (P.try $ score <$> parseRound)

parseRound2 :: Parser Int
parseRound2 = score . uncurry correct <$> parseRound
  where
    correct Rock Rock = (Rock, Scissors)
    correct Paper Rock = (Paper, Rock)
    correct Scissors Rock = (Scissors, Paper)
    correct a Paper = (a, a)
    correct Rock Scissors = (Rock, Paper)
    correct Paper Scissors = (Paper, Scissors)
    correct Scissors Scissors = (Scissors, Rock)

part2 :: Parser Int
part2 = sum <$> P.some (P.try parseRound2)

wrap :: (Show a) => Parser a -> T.Text -> T.Text
wrap p = T.pack . show . runParse p

day2 :: Day
day2 = Day (wrap part1) (wrap part2) (Just "15422") (Just "15442")
