module Main (main) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock

import Day
import Args

import Day1
import Day2

days :: [Day]
days = [day1, day2]

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
main = withConfig $ \Config{dayIndex,part,inputFile,measure} -> do
  let filename = case inputFile of
                   Just path -> path
                   Nothing -> "inputs/" <> show dayIndex <> ".txt"
  txt <- TIO.readFile filename
  let dayIdx = dayIndex - 1
      day = if dayIdx < length days then days !! dayIdx
                                    else error "Day not loaded"
      p1 = runPart measure (dayPart1 day txt) (part1Ans day)
      p2 = runPart measure (dayPart2 day txt) (part2Ans day)
  case part of
    Part1 -> p1
    Part2 -> p2
    Both -> p1 *> p2
