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
import Debug.Trace (trace, traceShowM)

import Day

type Stack = [Char]

type Dock = Seq Stack

parseDock :: Parser Dock
parseDock = fmap reverse <$> (firstRow >>= readRow) where
  box = P.between (C.string "[") (C.string "]") C.upperChar
  empty = C.string "   "
  mbBox = P.try (Just <$> box) <|> P.try (empty $> Nothing)
  row :: Parser [Maybe Char]
  row = mbBox `P.sepBy` C.string " "
  combine S.Empty [] = S.Empty
  combine (s :<| d) (Just v:ls) = (v:s) :<| combine d ls
  combine (s :<| d) (Nothing:ls) = s :<| combine d ls
  firstRow :: Parser Dock
  firstRow = do
    newBoxes <- row
    C.newline
    let emptyDock = S.fromList $ replicate (length newBoxes) []
    pure $ combine emptyDock newBoxes
  readRow :: Dock -> Parser Dock
  readRow d = do
    newBoxes <- row
    C.newline
    let d' = combine d newBoxes
        end = (C.string " " *> C.digitChar) $> d'
    P.try end <|> readRow d'

data Instr = Instr {move::Int, from::Int, to::Int}

parseInstr :: Parser Instr
parseInstr = do
  C.string "move "
  move <- L.decimal
  C.string " from "
  from <- L.decimal
  C.string " to "
  to <- L.decimal
  C.newline
  pure $ Instr move (from-1) (to-1)

parseInput :: Parser (Dock, [Instr])
parseInput = do
  d <- parseDock
  skipLine
  skipLine
  is <- P.some $ P.try parseInstr
  pure (d, is)
  where
    skipLine = P.skipManyTill C.printChar C.newline

part1 :: Parser String
part1 = do
  (d, is) <- parseInput
  let finalDock = foldl' f d is
  pure $ top finalDock
  where
    f dock Instr{move,from,to} = S.update to e' $ S.update from s' dock where
      s = dock `S.index` from
      s' = drop move s
      e = dock `S.index` to
      e' = reverse (take move s) <> e
    top = F.toList . fmap g
    g (a:_) = a
    g [] = ' '

part2 :: Parser String
part2 = do
  (d, is) <- parseInput
  let finalDock = foldl' f d is
  pure $ top finalDock
  where
    f dock Instr{move,from,to} = S.update to e' $ S.update from s' dock where
      s = dock `S.index` from
      s' = drop move s
      e = dock `S.index` to
      e' = take move s <> e
    top = F.toList . fmap g
    g (a:_) = a
    g [] = ' '

wrap :: Parser String -> Text -> Text
wrap p = T.pack . runParse p

day5 :: Day
day5 = mkDay "5" (wrap part1) (wrap part2)
