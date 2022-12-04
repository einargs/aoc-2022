module Day1 (day1) where

import qualified Data.Text as T
import qualified Text.Megaparsec as P
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Applicative (liftA2)
import Data.Functor (($>))
import Data.List (sortBy)

import Day

elf :: Parser Int
elf = end <|> num where
  end :: Parser Int
  end = C.newline $> 0
  num = liftA2 (+) (P.try $ L.decimal <* C.newline) elf

largest :: Parser Int
largest = f 0 where
  f :: Int -> Parser Int
  f i = g <|> pure i where
    g = do
      e <- P.try elf
      f $ max i e

top3 :: Parser Int
top3 = do
  elves <- P.some $ P.try elf
  pure $ sum $ take 3 $ sortBy (flip compare) elves

wrap :: (Show a) => Parser a -> T.Text -> T.Text
wrap p = T.pack . show . runParse p

day1 :: Day
day1 = answeredDay "1" (wrap largest) (wrap top3) "74394" "212836"
