import System.IO
import qualified Data.Map.Strict as Map

main = do
    contents <- readFile "out.txt"
    let list =  (filter ((>1) . length)) . Prelude.map (takeWhile (/='/')) . lines $ contents
    let list2 = unlines (list ++ ["a"] ++ ["i"])
    writeFile "out2.txt" list2


    import System.IO
    import Data.Map.Strict
    import Data.List
    
    lookup1 :: (Ord k)=> k -> Map k a -> Maybe a
    lookup1 = Data.Map.Strict.lookup
    take1 = Data.List.take
    drop1 = Data.List.drop
    filter1 = Data.List.filter
    
    main = do
        contents <- readFile "out2.txt"
        let list = lines contents
        let list2 = zip list (repeat True)
        let dict = fromList list2
        input <- readFile "test.txt"
        let output = (seg 1 input dict)
        case output of
            Just x -> writeFile "rec.txt" x
            Nothing -> writeFile "rec.txt" "Something is wrong"
    
    
    
    --seg :: Int-> String-> Map.Map String Bool -> Maybe String
    {-seg fstbr line dict = if (line == "") then Just ""
                     else if (fstbr > length line) then Nothing
                     else if ((lookup1 (Prelude.take fstbr line) dict) == Just True)
                        then case (seg 1 (Prelude.drop fstbr line) dict) of
                            Nothing -> seg (fstbr+1) line dict
                            Just x -> Just $ (Prelude.take fstbr line) ++ " " ++ x
                     else seg (fstbr+1) line dict-}
    
    --seg :: Int -> String -> [String] -> Maybe String
    --seg :: Int -> String -> [String] -> Maybe String
    seg fstbr line dict = if (line == "") then Just ""
                     else if (fstbr > length line) then Nothing
                     else if ((lookup1 (take1 fstbr line) dict)==Just True)
                        then case (seg 1 (drop1 fstbr line) dict) of
                            Nothing -> seg (fstbr+1) line dict
                            Just x -> Just $ (take1 fstbr line) ++ " " ++ x
                     else seg (fstbr+1) line dict
    
    
    
    
    

    