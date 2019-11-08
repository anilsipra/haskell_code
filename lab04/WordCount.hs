import Data.List
import Data.Char
import Data.Map.Strict hiding (filter,map)


-- takes a string and breaks into words in lower case as defined 
cwords :: String -> [Map String Int]
cwords str = if Data.List.null str then []
            else (fromList [((map toLower (fst (span p str))),1)]) 
            : cwords (dropWhile (not . p) (snd (span p str)))

-- predicate to check if in word as defined in the lab
p :: Char -> Bool
p = (pure (||) <*> (pure (||) <*> isLetter <*> (=='\'')) <*> (=='-'))


lmap :: String -> String
lmap = intercalate ['\n'] . map f2 . toAscList . unionsWith (+) . cwords 

f2 :: (String,Int) -> String
f2 (str,num) = str ++ " " ++ show num

main :: IO ()
main = interact $ lmap




