module Main where

    import Control.Monad.State
    import Data.Map (Map,(!),singleton,unionsWith)
    import System.Random
    import Data.List
    
    type RandState = State StdGen
    type Model = (String,Map String [Maybe String])
    
    buildModel :: [String] -> Model
    buildModel xs@(x:_) = (x,unionsWith (++) . transitions $ xs) where
        transitions (y:ys@(y':_)) = singleton y [Just y'] : transitions ys
        transitions [y] = [singleton y [Nothing]]
        transitions [] = error "Impossible error"
    buildModel [] = error "Empty model"
    
    runModel :: Model -> RandState [String]
    runModel (start,wordmap) = iter start where
        iter word = (word:) <$> do
            maybeNext <- select $ wordmap ! word
            case maybeNext of
                Just nextWord -> iter nextWord
                Nothing -> pure []
    
    roll :: Int -> RandState Int
    roll n = state $ randomR (1,n)
    
    select :: [a] -> RandState a
    select as = (as !!) . (subtract 1) <$> roll (length as)
    
    linefill :: Int -> [String] -> String
    linefill _ [] = "\n"
    linefill n (x:xs) = iter x xs where
        iter current (nextWord:ys)
            | length current + length nextWord + 1 > n = current ++ "\n" ++ linefill n (nextWord:ys)
            | otherwise                   = iter (current ++ " " ++ nextWord) ys
        iter current [] = current ++ "\n"
    
    main :: IO ()
    main = do
        input <- getContents
        gen   <- getStdGen
        let model = buildModel (words input)
            list = map (linefill 72) $ evalState (replicateM 1000 (runModel model)) gen
            maxl = length $ maximumBy (\s1 s2 -> compare (length s1) (length s2)) list
            minl = length $ minimumBy (\s1 s2 -> compare (length s1) (length s2)) list   
        putStr $ "max length =" ++ (show maxl) ++ "\n" ++ "min length =" ++ (show minl)
    
