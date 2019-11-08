import System.IO
import Data.Char
import qualified Data.Map.Strict as Map

main = do
    contents <- readFile "out.txt"
    let list =  (filter ((>1) . length)) . Prelude.map (takeWhile (/='/')) . lines $ contents
    let list2 = unlines . map (map toLower) $ (list ++ ["a"] ++ ["i"]++["is"])
    writeFile "rec.txt" list2



    
    
    
    

    