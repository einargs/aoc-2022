module Day12 (day12) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Functor (($>))
import Control.Monad (void)
import Data.ByteString qualified as BS
import Data.Foldable (foldl', toList)
import Algorithm.Search qualified as Alg
import Data.Maybe (fromJust, mapMaybe, listToMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Debug.Trace qualified as DT

import Day

type Pos = (Int, Int)

label :: Show a => String -> a -> a
label l v = DT.trace (l <> " " <> show v) v

-- length data start end
data Region = Region Int BS.ByteString Pos Pos

instance Show Region where
  show r@(Region l bs start end) = unlines ps where
    f p | p == start = 'S'
        | p == end = 'E'
        | otherwise = toEnum $ r#p + fromEnum 'a'
    h = regionHeight r
    mkRow y = f . (,y) <$> [0..l-1]
    ps = mkRow <$> [0..(h-1)]

regionHeight :: Region -> Int
regionHeight (Region w bs _ _) = BS.length bs `div` w

toRegion :: Text -> Region
toRegion t = Region w raw start end where
  (Just w) = T.findIndex (=='\n') t
  (Just si) = BS.elemIndex (toEnum $ fromEnum 'S') raw
  (Just ei) = BS.elemIndex (toEnum $ fromEnum 'E') raw
  start = (si `mod` w, si `div` w)
  end = (ei `mod` w, ei `div` w)
  raw = encodeUtf8 $ T.filter (/='\n') t

infixl 9 #
(#) :: Region -> Pos -> Int
(Region l bs _ _) # (x, y) = f $ bs `BS.index` ((l*y) + x) where
  f c | c == toEnum (fromEnum 'S') = 0
      | c == toEnum (fromEnum 'E') = 25
      | otherwise = fromEnum c - fromEnum 'a'

pathDist :: Region -> Pos -> Maybe Int
pathDist reg@(Region w _ _ end) start = length <$> answer where
  answer = Alg.bfs neighbors (==end) start
  neighbors p@(x,y) = filter f raw where
    raw = [(x-1, y), (x, y+1), (x, y-1), (x+1, y)]
    f p'@(x,y) = x >= 0 && y >= 0 && x < w && y < regionHeight reg
      && (reg # p') <= (reg # p) + 1 && p' /= p

part1 :: Region -> Int
part1 r@(Region _ _ start _) = fromJust $ pathDist r start

part2 :: Region -> Int
part2 r@(Region l _ _ _) = minimum $ mapMaybe (pathDist r) ps where
  f p = r#p == 0
  h = regionHeight r
  mkRow y = (,y) <$> [0..l-1]
  ps = filter f $ [0..(h-1)] >>= mkRow

wrap :: (Region -> Int) -> Text -> Text
wrap f = T.pack . show . f . toRegion

day12 :: Day
day12 = mkDay "12" (wrap part1) (wrap part2)
