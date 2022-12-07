module Day7 (day7) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.List qualified as L
import Text.Megaparsec ((<|>))
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L
import Data.Functor (($>))
import Debug.Trace qualified as DT

import Data.Tree (Tree(..))
import Data.Tree qualified as TR
import Data.Tree.Zipper (TreePos, Empty, Full)
import Data.Tree.Zipper qualified as Z
import Data.Maybe (fromJust)

import Day

type FS = Tree (Text, Int)

isDir :: FS -> Bool
isDir (Node _ []) = False
isDir _ = True

type FSZ = TreePos Full (Text, Int)

data CD = Up | Down Text


letters :: Parser Text
letters = T.pack <$> P.some (C.letterChar <|> C.char '.')

selectDir :: Text -> FSZ -> FSZ
selectDir n z = f $ Z.firstChild z where
  g c = n == n' where (n', _) = Z.label c
  f Nothing = error $ "couldn't find child named " <> T.unpack n
  f (Just c) | g c = c
             | otherwise = f $ Z.next c

parseCD :: Parser CD
parseCD = (C.string ".." $> Up) <|> (Down <$> letters)

performCDs :: FSZ -> Parser FSZ
performCDs z = do
  C.string "$ cd "
  cd <- parseCD
  C.newline
  let z' = case cd of
             Up -> case Z.parent z of
                     Just p -> p
                     Nothing -> error $ "no parent of " <> T.unpack (fst $ Z.label z)
             Down n -> selectDir n z
  (ls *> parseLS z') <|> performCDs z'
  where
    ls = P.try $ C.string "$ ls" *> C.newline

parseLS :: FSZ -> Parser FSZ
parseLS z = do
  listings <- P.some $ P.try listing
  let z' = Z.modifyTree (g listings) z
  (P.eof $> z') <|> performCDs z'
  where
    dirL = fmap (,0) $ C.string "dir " *> letters <* C.newline
    fileL = flip (,) <$> (L.decimal <* C.string " ") <*> (letters <* C.newline)
    listing = dirL <|> fileL
    makeChildren l = Node l []
    g ls (Node l cs') = Node l $ cs' <> (makeChildren <$> ls)

parseRoot :: Parser FS
parseRoot = do
  C.string "$ cd /\n$ ls\n"
  let root = Z.fromTree $ Node ("/", 0) []
  TR.foldTree getDirSizes . Z.toTree <$> parseLS root
  where
    getDirSizes (n, i) ls = Node (n, i + i') ls where
      i' = sum $ snd . rootLabel <$> ls

dropChildren :: FS -> [(Text, Int)]
dropChildren = TR.foldTree g where
  g _ [] = []
  g l ls = l : concat ls

part1 :: Parser Int
part1 = f <$> parseRoot where
  f = sum . filter (<=100000) . fmap snd . dropChildren

part2 :: Parser Int
part2 = f <$> parseRoot where
  f t = minimum $ filter (>=neededSpace) $ snd <$> dropChildren t where
    unusedSpace = 70000000 - snd (rootLabel t)
    neededSpace = 30000000 - unusedSpace

wrap :: Parser Int -> Text -> Text
wrap p = T.pack . show . runParse p

day7 :: Day
day7 = mkDay "7" (wrap part1) (wrap part2)
