{-# LANGUAGE EmptyDataDeriving #-}
module Day
  ( Day(..)
  , Parser
  , runParse
  , mkDay
  , answeredDay
  , dayFile
  ) where

import Data.Text (Text, unpack)
import qualified Text.Megaparsec as P

data Day = Day
  { dayName :: Text
  , dayFileName :: FilePath
  , dayPart1 :: Text -> Text
  , dayPart2 :: Text -> Text
  , part1Ans :: Maybe Text
  , part2Ans :: Maybe Text
  }

mkDay :: Text -> (Text -> Text) -> (Text -> Text) -> Day
mkDay n p1 p2 = Day n fn p1 p2 Nothing Nothing where
  fn = "inputs/" <> unpack n <> ".txt"

answeredDay :: Text -> (Text -> Text) -> (Text -> Text) -> Text -> Text -> Day
answeredDay n p1 p2 a1 a2 = (mkDay n p1 p2){part1Ans=Just a1, part2Ans=Just a2}

dayFile :: Day -> FilePath -> Day
dayFile d fn = d{dayFileName="inputs/" <> fn <> ".txt"}

data Void deriving (Eq,Ord,Show)

instance P.ShowErrorComponent Void where
  showErrorComponent _ = error "impossible"

runParse :: Parser a -> Text -> a
runParse p t = case P.parse p "" t of
                 Left e -> error $ P.errorBundlePretty e
                 Right v -> v

type Parser = P.Parsec Void Text
