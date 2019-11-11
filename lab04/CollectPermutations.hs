import Data.List
import Data.Char
import Data.Map.Strict 
import Control.Arrow

map1 = Prelude.map
map2 = Data.Map.Strict.map
take1 = Data.List.take
filter1 = Data.List.filter
filter2 = Data.Map.Strict.filter

--breaks up the sentence 
cwords :: String -> [Map String [String]]
cwords str = if str=="" then []
             else singleton (sort (map1 toLower (fst (span p str)))) 
                [(map1 toLower (fst (span p str)))] 
                  : cwords (dropWhile (not . p) (snd (span p str)))

--predicate to check word as defined in the lab
p :: Char -> Bool
p = (pure (||) <*> (pure (||) <*> isLetter <*> (=='\'')) <*> (=='-'))

prog2 :: String -> String
prog2 = cwords >>> unionsWith (++) >>> map2 (sort . nub) >>> filter2 ((>1) . length) 
        >>> map2 (intercalate [',',' ']) >>> elems 
        >>> sortOn (takeWhile (/=',')) >>> intercalate ['\n']
        
main :: IO ()        
main = interact $ prog2



