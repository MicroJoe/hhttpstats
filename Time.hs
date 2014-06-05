module Time
( toTimestamp
, timeDiff
, TimeResolution (..)
) where

import System.Locale
import Data.Maybe

import Data.Time
import Data.Time.Format

type Timestamp = Int

data TimeResolution = Second | Minute | Hour | Day
  deriving (Eq, Show)

resolutionTable :: [(TimeResolution, Int)]
resolutionTable = [
    (Second, 1),
    (Minute, 60),
    (Hour, 3600),
    (Day, 3600 * 24)
  ]

timeDiff :: TimeResolution -> Timestamp -> Timestamp -> Int
timeDiff res ref t = quot diff coef
  where
    diff = t - ref
    coef = fromMaybe 1 $ lookup res resolutionTable

toTimestamp :: UTCTime -> Timestamp
toTimestamp = read . formatTime defaultTimeLocale "%s"
