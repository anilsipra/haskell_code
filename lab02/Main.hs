module Main where

import System.IO
import Data.List
import Intervals
import SimpleTime

-- CHANGE EndpointType to be Int for testing, but make sure to change it back
-- for submission!

type EndpointType = Time
-- type EndpointType = Int

---- Input/Output Interface ----

-- Example: splitOn ',' "abc,def,ghi"  => ["abc", "def", "ghi"]
splitOn :: Char -> String -> [String]
splitOn splitChar []    = [[]]
splitOn splitChar (headChar:restChars)
  | splitChar == headChar = [[]] ++ splitOn splitChar restChars
  | otherwise             = (headChar:currentWord):restWords
  where
    currentWord:restWords = splitOn splitChar restChars

-- Vanilla Haskell doesn't allow instances on type synonyms,
-- so we can't make customized Show/Read instances.

readIS :: String -> IntervalSet EndpointType
readIS = map read . splitOn ','

showIS :: IntervalSet EndpointType -> String
showIS = concat . intersperse "," . map show . normalizeIS

-- Combine touching/overlapping regions and remove NoTime records.
-- Inverting twice effectively combines overlapping regions.
normalizeIS :: (Ord a) => IntervalSet a -> IntervalSet a
normalizeIS s
  | simplified == [] = emptyIS
  | otherwise        = simplified
  where
    inverse = difference allIS
    simplified = sort . filter (/= Empty) . inverse . inverse $ s

processLine :: String -> String
processLine line =
  -- Example: words "abc def ghi" => ["abc", "def", "ghi"]
  case words line of
    "intersection":rest -> showIS . normalizeIS . intersectAll $ map readIS rest -- hint: These can be done in one line
    "union":rest        -> showIS . normalizeIS . unionAll $ map readIS rest-- further hint: think map and ($) and (.)
    "difference":rest   -> showIS . normalizeIS . differenceAll $ map readIS rest
    "disjoint":rest     -> show  . areAllDisjoint $ map readIS rest
    "equal":rest        -> show  . areAllEqual $ map readIS rest
    _                   -> "Invalid input"





main :: IO ()
main = do
  input <- getLine
  case input of 
    'q':_ -> return ()
    _     -> do
      putStrLn . processLine $ input
      hFlush stdout
      main