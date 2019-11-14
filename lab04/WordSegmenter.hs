module WordSegmenter where 

import System.IO
import Data.Map.Strict
import Data.List

{-this is the code used to clean the dictionary downloaded from the SCOWL website.
 dict.txt contains the cleaned dictionary. I have used the custom dictionary provided. Just provide
 the file and it should work.
 If you want to use the scowl dictionary, it has been provided as a text file dict.txt 
 The code just removes unwanted characters, filters all the 
nonsensical single letter words and adds 'a', 'i', 'is' to the dictionary. 

import System.IO
import Data.Char
import qualified Data.Map.Strict as Map

main = do
    contents <- readFile "out.txt"
    let list =  (filter ((>1) . length)) . Prelude.map (takeWhile (/='/')) . lines $ contents
    let list2 = unlines . map (map toLower) $ (list ++ ["a"] ++ ["i"]++["is"])
    writeFile "dict.txt" list2

-}

lookup1 :: (Ord k)=> k -> Map k a -> Maybe a
lookup1 = Data.Map.Strict.lookup
take1 = Data.List.take
drop1 = Data.List.drop
filter1 = Data.List.filter

main :: IO ()
main = do
    contents <- readFile "dict.txt"   -- change according to the file used
    let list = lines contents
    let list2 = zip list (repeat True)
    let dict = fromList list2
<<<<<<< HEAD
    input <- getLine
=======
    input <- getContents
>>>>>>> 41fd406408a12be32946eaba0f69f627d77bf89c
    let output = (seg (length input) input dict)
    case output of
        Just x -> putStrLn x
        Nothing -> putStrLn "You did something is wrong"


seg :: Int -> [Char] -> Map [Char] Bool -> Maybe [Char]
seg fstbr line dict = if (line == "") then Just ""
                 else if (fstbr < 1) then Nothing
                 else if ((lookup1 (take1 fstbr line) dict)==Just True)
                    then case (seg (length (drop1 fstbr line)) (drop1 fstbr line) dict) of
                        Nothing -> seg (fstbr-1) line dict
                        Just x -> Just $ (take1 fstbr line) ++ " " ++ x
                 else seg (fstbr-1) line dict