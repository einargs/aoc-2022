module Main (main) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock
import Data.Map (Map)
import Data.Map qualified as M

import Day
import Args

import Day1
import Day2
import Day2Golf
import Day3

days :: Map Text Day
days = M.fromList $ f <$> dayList where
  f day@Day{dayName} = (dayName, day)
  dayList = [day1, day2, day2Golf, day3]

runPart :: Bool -> Text -> Maybe Text -> IO ()
runPart measure ans mbAns = do
  t0 <- getCurrentTime
  ans `seq` pure ()
  t1 <- getCurrentTime
  TIO.putStrLn $ ans <> ansTag <> timeTag t0 t1
  where
    ansTag = case mbAns of
            Just ans' | ans' == ans -> " (Correct)"
                      | otherwise -> " (Wrong)"
            Nothing -> ""
    timeTag t0 t1
      | measure = " (TIME: " <> T.pack (show (diffUTCTime t1 t0)) <> ")"
      | otherwise = ""

main :: IO ()
main = withConfig $ \Config{dayName,part,inputFile,measure} -> do
  let day = case M.lookup dayName days of
              Just day -> day
              Nothing -> error "Day not loaded"
  let filename = case inputFile of
                   Just path -> path
                   Nothing -> "inputs/" <> T.unpack dayName <> ".txt"
  txt <- TIO.readFile filename
  let p1 = runPart measure (dayPart1 day txt) (part1Ans day)
      p2 = runPart measure (dayPart2 day txt) (part2Ans day)
  case part of
    Part1 -> p1
    Part2 -> p2
    Both -> p1 *> p2
