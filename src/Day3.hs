module Day3 (day3) where

import Data.Text qualified as T
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as C
import Data.Char (ord)
import Data.Set (Set)
import Data.Set qualified as S

import Day

fromSingleton :: Show a => Set a -> a
fromSingleton s = case S.toList s of
                       [x] -> x
                       v -> error $ show v

-- Only valid for letters
conv :: Char -> Int
conv c | c >= 'a' = 1 + ord c - ord 'a'
       | otherwise = 27 + ord c - ord 'A'

part1 :: T.Text -> T.Text
part1 = T.pack . show . sum . fmap f . T.splitOn "\n" . T.init where
  f t = fromSingleton $ S.intersection (con a) (con b) where
    (a, b) = T.splitAt (T.length t `div` 2) t
    con x = S.fromList $ conv <$> T.unpack x

parseGroup :: Parser Int
parseGroup = do
  a <- parseBag
  b <- parseBag
  c <- parseBag
  let joint = S.intersection a $ S.intersection b c
  pure $ fromSingleton joint
  where
    parseBag :: Parser (Set Int)
    parseBag = do
      sack <- P.some C.letterChar
      C.space
      pure $ S.fromList $ conv <$> sack

part2 :: T.Text -> T.Text
part2 = T.pack . show . sum . runParse (P.some $ P.try parseGroup)

day3 :: Day
day3 = Day "3" part1 part2 (Just "7845") (Just "2790")
