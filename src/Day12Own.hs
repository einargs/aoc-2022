module Day12Own (day12Own) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString qualified as BS
import Data.Sequence (Seq((:|>), (:<|), Empty))
import Data.Sequence qualified as S
import Data.Map (Map)
import Data.Map qualified as M

import Day

type Pos = (Int, Int)

-- length data start end
data Region = Region Int BS.ByteString Pos Pos

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

-- | We want a shortest path tree from end to all 0 elevations.
spt :: Region -> Map Pos Int
spt r@(Region w _ _ end) = onlyGround tree where
  onlyGround = M.filterWithKey \p _ -> r#p == 0
  tree = go (M.singleton end 0) (S.singleton (end, 0))
  go :: Map Pos Int -> Seq (Pos, Int) -> Map Pos Int
  go visited Empty = visited
  go visited ((p@(x,y), i) :<| queue) = process neighbors visited queue where
    process :: [Pos] -> Map Pos Int -> Seq (Pos, Int) -> Map Pos Int
    process [] v q = go v q
    process (n:ns) v q = process ns (M.insert n (i+1) v) (q :|> (n, i+1))
    neighbors = filter f raw where
      raw = [(x-1, y), (x, y+1), (x, y-1), (x+1, y)]
      f p'@(x,y) = x >= 0 && y >= 0 && x < w && y < regionHeight r
        && (r # p) <= (r # p') + 1 && p' /= p && p' `M.notMember` visited

part1 :: Region -> Int
part1 r@(Region _ _ start _) = spt r M.! start

part2 :: Region -> Int
part2 r = minimum $ M.elems $ spt r

wrap :: (Region -> Int) -> Text -> Text
wrap f = T.pack . show . f . toRegion

day12Own :: Day
day12Own = answeredDay "12Own" (wrap part1) (wrap part2) "456" "454"
  `dayFile` "12"
