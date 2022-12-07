module Day7 (day7) where

import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec ((<|>))
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L
import Data.Functor (($>))
import Control.Monad (void)
import Data.Tree (Tree(..))
import Data.Tree qualified as TR
import Data.Tree.Zipper (TreePos, Full)
import Data.Tree.Zipper qualified as Z

import Day

type FS = Tree (Text, Int)

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
  void $ C.string "$ cd "
  cd <- parseCD
  void C.newline
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
  void $ C.string "$ cd /\n$ ls\n"
  let root = Z.fromTree $ Node ("/", 0) []
  TR.foldTree getDirSizes . Z.toTree <$> parseLS root
  where
    getDirSizes (n, i) ls = Node (n, i + i') ls where
      i' = sum $ snd . rootLabel <$> ls

dropChildren :: FS -> [(Text, Int)]
dropChildren = TR.foldTree g where
  g _ [] = []
  g l ls = l : concat ls

part1 :: FS -> Int
part1 = sum . filter (<=100000) . fmap snd . dropChildren

part2 :: FS -> Int
part2 t = minimum $ filter (>=neededSpace) $ snd <$> dropChildren t where
  unusedSpace = 70000000 - snd (rootLabel t)
  neededSpace = 30000000 - unusedSpace

wrap :: (FS -> Int) -> Text -> Text
wrap f = T.pack . show . f . runParse parseRoot

day7 :: Day
day7 = answeredDay "7Zipper" (wrap part1) (wrap part2) "1391690" "5469168" `dayFile` "7"
