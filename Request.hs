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

import qualified Data.ByteString.Lazy.Char8 as BS

-- IP Address
type IPAddr = (Word8, Word8, Word8, Word8)

-- Method
data Method = Get | Post | Unknown deriving (Show, Eq)

-- Request
data Request = Request {
    ip :: IPAddr,
    method :: Method,
    date :: UTCTime
} deriving (Show)

-- Reading from ByteString

readIPAddr :: BS.ByteString -> Maybe IPAddr
readIPAddr s = do
  let skipRead = BS.readInt . BS.tail
  (n1, r1) <- BS.readInt s
  (n2, r2) <- skipRead r1
  (n3, r3) <- skipRead r2
  (n4, r4) <- skipRead r3
  return (fromIntegral n1, fromIntegral n2, fromIntegral n3, fromIntegral n4)

readMethod :: BS.ByteString -> Maybe Method
readMethod = flip lookup bstable
  where bstable = map (\(a,b) -> (BS.pack a, b)) table
        table = [("GET", Get), ("POST", Post)]

readDate :: BS.ByteString -> Maybe UTCTime
readDate = parseTime defaultTimeLocale dateFmt . BS.unpack
  where dateFmt = "%d/%b/%Y:%H:%M:%S"

readRequest :: BS.ByteString -> Maybe Request
readRequest s = do
  ip <- readIPAddr s
  method <- readMethod . BS.takeWhile (/= ' ') . dropWhile' (/= '"') $ s
  date <- readDate . BS.takeWhile (/= ' ') . dropWhile' (/= '[') $ s
  return Request { ip = ip, method = method, date = date }
  where
    dropWhile' p = BS.drop 1 . BS.dropWhile p
