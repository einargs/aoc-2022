{-# LANGUAGE EmptyDataDeriving #-}
module Day where

import Data.Text (Text)
import qualified Text.Megaparsec as P

data Day = Day
  { dayName :: Text
  , dayPart1 :: Text -> Text
  , dayPart2 :: Text -> Text
  , part1Ans :: Maybe Text
  , part2Ans :: Maybe Text
  }

data Void deriving (Eq,Ord,Show)

instance P.ShowErrorComponent Void where
  showErrorComponent _ = error "impossible"

runParse :: Parser a -> Text -> a
runParse p t = case P.parse p "" t of
                 Left e -> error $ P.errorBundlePretty e
                 Right v -> v

type Parser = P.Parsec Void Text
