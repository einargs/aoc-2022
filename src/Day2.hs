module Day2 (day2) where

import qualified Data.Text as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C
import Data.Functor (($>))

import Day

data RPS = Rock | Paper | Scissors deriving (Eq, Show, Enum, Bounded)

enumCycle :: forall a. (Bounded a, Enum a) => Int -> a -> a
enumCycle i v = toEnum $ (fromEnum v + i) `mod` (fromEnum (maxBound :: a) + 1)

winsAgainst :: RPS -> RPS
winsAgainst = enumCycle 1

losesAgainst :: RPS -> RPS
losesAgainst = enumCycle (-1)

rpsScore :: RPS -> Int
rpsScore rps = fromEnum rps + 1

data Outcome = Lose | Draw | Win deriving (Eq, Show, Enum)

outcomeScore :: Outcome -> Int
outcomeScore o = fromEnum o * 3

type Round = (RPS, RPS)

roundOutcome :: Round -> Outcome
roundOutcome (a, b)
  | a == b = Draw
  | b == winsAgainst a = Win
  | otherwise = Lose

roundScore :: Round -> Int
roundScore r@(_,b) = rpsScore b + outcomeScore (roundOutcome r)

parseEnum :: forall a. Enum a => [T.Text] -> Parser a
parseEnum ls = P.choice (f <$> zip ls [0..]) <* C.space where
  f (s, v) = C.string s $> toEnum v

parseABC :: Enum a => Parser a
parseABC = parseEnum ["A", "B", "C"]

parseXYZ :: Enum a => Parser a
parseXYZ = parseEnum ["X", "Y", "Z"]

parseRound :: Parser Round
parseRound = (,) <$> parseABC <*> parseXYZ

part1 :: Parser Int
part1 = sum <$> P.some (P.try $ roundScore <$> parseRound)

parseRound2 :: Parser Int
parseRound2 = do
  elf <- parseABC
  outcome <- parseXYZ
  let me = enumCycle (fromEnum outcome - 1) elf
  pure $ outcomeScore outcome + rpsScore me

part2 :: Parser Int
part2 = sum <$> P.some (P.try parseRound2)

wrap :: (Show a) => Parser a -> T.Text -> T.Text
wrap p = T.pack . show . runParse p

day2 :: Day
day2 = answeredDay "2" (wrap part1) (wrap part2) "15422" "15442"
