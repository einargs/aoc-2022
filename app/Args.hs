module Args
  ( Part(..)
  , Config(..)
  , withConfig
  ) where

import Options.Applicative

data Part = Part1 | Part2 | Both

readPart :: ReadM Part
readPart = maybeReader $ \case
  "part1" -> Just Part1
  "part2" -> Just Part2
  "both" -> Just Both
  _ -> Nothing

data Config = Config
  { dayIndex :: Int
  , part :: Part
  , inputFile :: Maybe FilePath
  , measure :: Bool
  }

config :: Parser Config
config = Config
  <$> argument auto (help "the day of the problem" <> metavar "DAY")
  <*> argument readPart
    (help "the part of that day's problem" <> metavar "PART")
  <*> optional (strOption
    (  long "file"
    <> short 'f'
    <> help "the input file to use"
    <> metavar "INPUT_FILE") )
  <*> switch
    (  long "measure"
    <> short 'm'
    <> help "Measure the duration of the executed parts?")

withConfig :: (Config -> IO ()) -> IO ()
withConfig f = execParser opts >>= f where
  opts = info (config <**> helper)
    (  fullDesc
    <> progDesc "Run solutions for different days of advent of code")
