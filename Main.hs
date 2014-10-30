import System.IO
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Lazy.Char8 as BS

import Request
import Stats
import Time

import Data.Maybe

main :: IO ()
main = do
  contents <- BS.getContents
  let result = process contents
  mapM_ (putStrLn) result

process :: BS.ByteString -> [String]
process s =
    map (\(a,b) -> (show a) ++ "\t" ++ (show b)) $ Map.toList freq
  where
    requests = sortByDate . catMaybes . map readRequest . BS.split '\n' $ s
    refdate = toTimestamp . date . head $ requests
    freq = frequency . map (timeDiff Hour refdate . toTimestamp . date) $ requests
