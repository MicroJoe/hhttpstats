module Request
( IPAddr
, Method
, Request
, date
, ip
, method
, readRequest
) where

import System.Locale

import Data.Maybe
import Data.Binary

import Data.Time
import Data.Time.Format

import Data.List
import Data.List.Split

-- IP Address
type IPAddr = (Word8, Word8, Word8, Word8)

readIPAddr :: String -> IPAddr
readIPAddr = toTup . map read . splitOn "."
    where toTup [a, b, c, d] = (a, b, c, d)

-- Method
data Method = Get | Post | Unknown deriving (Show, Eq)

readMethod :: String -> Method
readMethod = fromMaybe Unknown . flip lookup table
    where table = [("GET", Get), ("POST", Post)]

-- Request
data Request = Request {
    ip :: IPAddr,
    method :: Method,
    date :: UTCTime
} deriving (Show)

readRequest :: String -> Request
readRequest s =
  Request {
    ip = ip,
    method = method,
    date = date
  }
  where
    dropWhile' p l = drop 1 $ dropWhile p l
    dateFmt = "%d/%b/%Y:%H:%M:%S"

    extractMethod = takeWhile (/= ' ') . dropWhile' (/= '"')
    extractDateString = takeWhile (/= ' ') . dropWhile' (/= '[')
    extractDate = parseTime defaultTimeLocale dateFmt . extractDateString
    extractIP = takeWhile (/= ' ')

    ip = readIPAddr . extractIP $ s
    date = fromJust . extractDate $ s
    method = readMethod . extractMethod $ s
