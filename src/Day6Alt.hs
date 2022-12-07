module Day6Alt (day6Alt) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.List qualified as L
import Control.Applicative (ZipList(..))
import Data.Maybe (fromJust)

import Day

solve :: Int -> Text -> Text
solve len = T.pack . show . f . T.unpack where
  f = (+len) . fromJust . L.findIndex isCorrect . windows
  windows = getZipList . traverse ZipList . take len . L.tails
  isCorrect = (len ==) . length . L.nub

day6Alt :: Day
day6Alt = answeredDay "6Alt" (solve 4) (solve 14) "1042" "2980" `dayFile` "6"
