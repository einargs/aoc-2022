module Day3 (day3) where

import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec qualified as P
import Text.Megaparsec ((<|>))
import Text.Megaparsec.Char qualified as C
import Data.Functor (($>))
import Data.Char (ord)
import Data.Set (Set)
import qualified Data.Set as S

import Day

type Bag = (Set Int, Set Int)

part1 :: [Bag] -> Int
part1 ls = sum $ f <$> ls where
  f :: Bag -> Int
  f (a, b) = x where
    x = case S.toList $ S.intersection a b of
          [x] -> x
          v -> error $ show v

wrap1 :: ([Bag] -> Int) -> T.Text -> T.Text
wrap1 f = T.pack . show . f . fmap parse . T.splitOn "\n" . T.init where
  parse t = let (a, b) = sp t in (con a, con b) where
                con x = S.fromList $ conv <$> T.unpack x
  sp t = T.splitAt (T.length t `div` 2) t
  conv c | c >= 'a' = 1 + ord c - ord 'a'
         | otherwise = 27 + ord c - ord 'A'

parseGroup :: Parser Int
parseGroup = do
  a <- parseBag
  b <- parseBag
  c <- parseBag
  let joint = S.intersection a $ S.intersection b c
  case S.toList joint of
    [x] -> pure x
    v -> error $ show v
  where
    parseBag :: Parser (Set Int)
    parseBag = do
      sack <- P.some C.letterChar
      C.space
      pure $ S.fromList $ conv <$> sack
    conv c | c >= 'a' = 1 + ord c - ord 'a'
           | otherwise = 27 + ord c - ord 'A'

part2 :: T.Text -> T.Text
part2 = T.pack . show . sum . runParse (P.some $ P.try parseGroup)

day3 :: Day
day3 = Day "3" (wrap1 part1) part2 Nothing Nothing
