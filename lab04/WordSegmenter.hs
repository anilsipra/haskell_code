import System.IO


main = do
    contents <- readFile "out.txt"
    let dict = lines contents
    input <- getContents
    let output = seg 1 input dict
    case output of
        Just x -> putStrLn x
    
    



seg :: Int -> String -> [String] -> Maybe String
seg fstbr line dict = if (line == "") then Just ""
                 else if (fstbr > length line) then Nothing
                 else if ((take1 fstbr line) `elem` dict)
                    then case (seg 1 (drop1 fstbr line) dict) of
                        Nothing -> seg (fstbr+1) line dict
                        Just x -> Just $ (take1 fstbr line) ++ " " ++ x
                 else seg (fstbr+1) line dict


                 seg fstbr line dict = if (line == "") then Just ""
                                  else if (fstbr > length line) then Nothing
                                  else if ((lookup1 (take fstbr line) dict) == Just True)
                                    then case (seg 1 (drop fstbr line) dict) of
                                        Nothing -> seg (fstbr+1) line dict
                                        Just x -> Just $ (Prelude.take fstbr line) ++ " " ++ x
                                  else seg (fstbr+1) line dict