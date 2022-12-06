module Day5 (day5) where

import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec ((<|>))
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L
import Data.Sequence (Seq((:<|)))
import Data.Sequence qualified as S
import Data.Functor (($>))
import Data.List (foldl')
import Data.Foldable qualified as F

import Day

type Stack = [Char]

type Dock = Seq Stack

parseDock :: Parser Dock
parseDock = fmap reverse <$> readRow firstRowF where
  box = P.between (C.string "[") (C.string "]") C.upperChar
  empty = C.string "   "
  mbBox = P.try (Just <$> box) <|> P.try (empty $> Nothing)
  row :: Parser [Maybe Char]
  row = mbBox `P.sepBy` C.string " " <* C.newline
  combine S.Empty [] = S.Empty
  combine (s :<| d) (Just v:ls) = (v:s) :<| combine d ls
  combine (s :<| d) (Nothing:ls) = s :<| combine d ls
  firstRowF boxes = S.fromList $ replicate (length boxes) []
  readRow :: ([Maybe Char] -> Dock) -> Parser Dock
  readRow f = do
    newBoxes <- row
    let d' = combine (f newBoxes) newBoxes
    P.try (readRow $ const d') <|> pure d'

data Instr = Instr {move::Int, from::Int, to::Int}

parseInstr :: Parser Instr
parseInstr = Instr <$> n "move " <*> sub (n " from ")
    <*> sub (n " to ") where
  n s = C.string s *> L.decimal
  sub p = subtract 1 <$> p

parseInput :: Parser (Dock, [Instr])
parseInput = do
  d <- parseDock
  skipLine <* skipLine
  is <- parseInstr `P.sepEndBy` C.newline
  pure (d, is)
  where skipLine = P.skipManyTill C.printChar C.newline

solve :: (forall a. [a] -> [a]) -> Parser String
solve h = top . uncurry (foldl' f) <$> parseInput where
  f dock Instr{move,from,to} = S.adjust toF to $ S.adjust fromF from dock
    where fromF = drop move
          toF e = h (take move (dock `S.index` from)) <> e
  top = F.toList . fmap head

part1 :: Parser String
part1 = solve reverse

part2 :: Parser String
part2 = solve id

wrap :: Parser String -> Text -> Text
wrap p = T.pack . runParse p

day5 :: Day
day5 = answeredDay "5" (wrap part1) (wrap part2) "RNZLFZSJH" "CNSFCGJSM"
