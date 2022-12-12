module Day12 (day12) where

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
import Data.Foldable (foldl', toList)

import Day

part1 :: Parser Int
part1 = undefined

part2 :: Parser Int
part2 = undefined

wrap :: Show a => Parser a -> Text -> Text
wrap p = T.pack . show . runParse p

day12 :: Day
day12 = mkDay "12" (wrap part1) (wrap part2)
