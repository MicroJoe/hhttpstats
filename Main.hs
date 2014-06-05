import System.IO
import qualified Data.Map.Strict as Map

import Request
import Stats
import Time

main :: IO ()
main = do
  process stdin []

process :: Handle -> [String] -> IO ()
process h xs = do
  ineof <- hIsEOF h
  if ineof
    then do
      result <- return $ sortByDate . map readRequest $ xs
      ref <- return $ toTimestamp . date . head $ result
      freq <- return $ Map.toList . frequency . map (timeDiff Hour ref . toTimestamp . date) $ result
      mapM_ (putStrLn . show) $ freq
    else do
      line <- hGetLine h
      process h (line:xs)
