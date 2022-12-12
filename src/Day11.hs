module Day11 (day11) where

import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec ((<|>))
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L
import Data.Functor (($>))
import Control.Monad (void)
import Data.Sequence (Seq((:<|), (:|>)))
import Data.Sequence qualified as S
import Data.List (sortBy, )
import Data.Foldable (foldl', toList)

import Day

data Monkey = Monkey
  { mItems :: Seq Int
  , mOperation :: Int -> Int
  , mTest :: Int -> Int
  , mDivisor :: Int
  , mInspections :: Int
  }

instance Show Monkey where
  show Monkey{mItems,mDivisor,mInspections} = "Monkey " <> unwords [show mItems, show mDivisor, show mInspections]

type Troop = Seq Monkey

data MExpr = Old | Lit Int

data MOp = Add MExpr MExpr | Mult MExpr MExpr

executeMOp :: Int -> MOp -> Int
executeMOp i = \case
                Add e1 e2 -> eval e1 + eval e2
                Mult e1 e2 -> eval e1 * eval e2
  where eval Old = i
        eval (Lit n) = n

parseMonkey :: Parser Monkey
parseMonkey = do
  void $ C.string "Monkey " *> L.decimal *> C.string ":" *> C.newline
  void $ C.string "  Starting items: "
  items <- L.decimal `P.sepBy` C.string ", "
  void $ C.newline *> C.string "  Operation: new = "
  op <- parseOp
  void $ C.newline *> C.string "  Test: divisible by "
  divisibleBy <- L.decimal
  void $ C.newline *> C.string "    If true: throw to monkey "
  trueIndex <- L.decimal
  void $ C.newline *> C.string "    If false: throw to monkey "
  falseIndex <- L.decimal
  let mTest i = if i `mod` divisibleBy == 0 then trueIndex else falseIndex
  pure $ Monkey (S.fromList items) op mTest divisibleBy 0
  where
    parseMExpr = (C.string "old" $> Old) <|> (Lit <$> L.decimal)
    parseMOp = do
      e1 <- parseMExpr <* C.space
      op <- (C.string "*" $> Mult) <|> (C.string "+" $> Add)
      e2 <- C.space *> parseMExpr
      pure $ op e1 e2
    parseOp = flip executeMOp <$> parseMOp

parseTroop :: Parser Troop
parseTroop = fmap S.fromList $ parseMonkey `P.sepBy` P.try (C.newline *> C.newline)

performRound :: Bool -> Troop -> Troop
performRound div3 troop = foldl' f troop [0..(S.length troop - 1)] where
  gcd = product $ mDivisor <$> troop
  throwItemTo :: Int -> Int -> Troop -> Troop
  throwItemTo item i = S.adjust' g i where
    g m@Monkey{mItems} = m{mItems=mItems :|> item}
  f :: Troop -> Int -> Troop
  f trp i = S.update i m' $ foldl' inspect trp mItems where
    inspect trp' item = throwItemTo item' target trp' where
      item' = if div3 then mOperation item `div` 3 else mOperation item `mod` gcd
      target = mTest item'
    m' = Monkey S.empty mOperation mTest mDivisor $ mInspections + S.length mItems
    Monkey{mItems, mOperation, mTest, mDivisor, mInspections} = trp `S.index` i

performRounds :: Int -> Bool -> Troop -> Troop
performRounds 0 div3 trp = trp
performRounds i div3 trp = performRounds (i-1) div3 $ performRound div3 trp

part1 :: Troop -> Int
part1 troop = a * b where
  (a:b:_) = sortBy (flip compare) inspections
  inspections = mInspections <$> toList troop'
  troop' = performRounds 20 True troop

part2 :: Troop -> Int
part2 troop = a*b where
  (a:b:_) = sortBy (flip compare) inspections
  inspections = mInspections <$> toList troop'
  troop' = performRounds 10000 False troop

wrap :: Show a => (Troop -> a) -> Text -> Text
wrap f = T.pack . show . f . runParse parseTroop

day11 :: Day
day11 = mkDay "11" (wrap part1) (wrap part2)
