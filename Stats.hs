module Stats
( frequency
, sortByDate
) where

import Data.List
import Data.Ord
import Data.Map.Strict

import Request

frequency :: Ord a => [a] -> Map a Int
frequency l = frequency' l empty

frequency' :: Ord a => [a] -> Map a Int -> Map a Int
frequency' (x:xs) m = frequency' xs $ insertWith (+) x 1 m
frequency' [] m = m

sortByDate :: [Request] -> [Request]
sortByDate = sortBy (comparing date)
