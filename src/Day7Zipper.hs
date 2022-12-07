module Day7Zipper (day7Zipper) where

import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec ((<|>))
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L
import Data.Functor (($>))
import Control.Monad (void)
import Data.Maybe (catMaybes)

import Day

data FS = Dir [FS] | File Int deriving Show

data Crumb = Crumb [FS] [FS] deriving Show

type FSZ = (FS, [Crumb])

data CD = Up | Down

parent :: FSZ -> Maybe FSZ
parent (_, []) = Nothing
parent (i, Crumb ls rs:cs) = Just (Dir $ ls <> [i] <> rs, cs)

descend :: FS -> FSZ -> FSZ
descend _ (File _, _) = error "No children of files"
descend child (Dir ls, cs) = (child, Crumb ls []:cs)

toZipper :: FS -> FSZ
toZipper fs = (fs, [])

fromZipper :: FSZ -> FS
fromZipper z = case parent z of
                 Just p -> fromZipper p
                 Nothing -> fst z

addChildren :: [FS] -> FSZ -> FSZ
addChildren _ (File _, _) = error "No children of files"
addChildren cs (Dir cs', crumbs) = (Dir $ cs' <> cs, crumbs)

name :: Parser Text
name = T.pack <$> P.some (C.letterChar <|> C.char '.')

performCDs :: FSZ -> Parser FSZ
performCDs z = do
  z' <- apply <$> parseCD
  (ls *> parseLS z') <|> performCDs z'
  where
    parseCD = C.string "$ cd " *> p <* C.newline where
      p = (C.string ".." $> Up) <|> (name $> Down)
    ls = C.string "$ ls" *> C.newline
    apply Up = case parent z of
                 Just p -> p
                 Nothing -> error "no parent"
    apply Down = descend (Dir []) z

parseLS :: FSZ -> Parser FSZ
parseLS z = do
  z' <- flip addChildren z <$> listings
  (P.eof $> z') <|> performCDs z'
  where
    dirL = C.string "dir " *> name *> C.newline $> Nothing
    fileL = Just <$> L.decimal <* C.string " " <* name <* C.newline
    listing = dirL <|> fileL
    listings = fmap File . catMaybes <$> P.some listing

parseRoot :: Parser FS
parseRoot = do
  void $ C.string "$ cd /\n$ ls\n"
  fromZipper <$> parseLS (toZipper $ Dir [])

dirSizes :: FS -> [Int]
dirSizes = snd . f where
  f :: FS -> (Int, [Int])
  f (File i) = (i, [])
  f (Dir is) = (v, v:concat (snd <$> is')) where
    v = sum $ fst <$> is'
    is' = f <$> is

wrap :: ([Int] -> Int) -> Text -> Text
wrap f = T.pack . show . f . dirSizes . runParse parseRoot

day7Zipper :: Day
day7Zipper = answeredDay "7" (wrap part1) (wrap part2) "1391690" "5469168" where
  part1 = sum . filter (<=100000)
  part2 fs = minimum $ filter (>=(head fs - 40000000)) fs
