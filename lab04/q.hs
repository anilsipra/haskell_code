import System.IO 
import WordSegmentLib
import Data.Map.Strict

main :: IO ()
main = do
    list <- lines <$> readFile "rec.txt"
    let dict = fromList (zip list (repeat True))
    input <- getContents
    let output = seg (length input) input dict
    case output of 
        Just x -> putStrLn x
        Nothing -> putStrLn "You have done something wrong"
    
    