import RandState
import Control.Monad
import System.Random
import System.IO
import System.Environment

randR :: Random a => (a, a) -> RandState a
randR (l,h) = do
    gen <- get
    let (x, gen') = randomR (l,h) gen
    put gen'
    return x

piTrial :: RandState Bool
piTrial = do
    x <- randR (-1.0,1.0)
    y <- randR (-1.0,1.0)
    return (withincircle (x,y))

withincircle :: (Float,Float) -> Bool
withincircle (x,y) = ((sqrt (x^2 + y^2)) <= 1)

bernoulliTrials :: Int -> RandState Bool -> RandState Int
bernoulliTrials nTimes piTrial = length <$> (filter id <$>  (replicateM nTimes piTrial)) 
    
approxPi :: Int -> RandState Double
approxPi n = (*4) <$> ((/) <$> (fromIntegral <$> (bernoulliTrials n piTrial)) <*> (fromIntegral <$> (pure n)))

main :: IO ()
main = do
    gen  <- newStdGen
    args <- getArgs
    case args of
        [num] ->  putStr . show . fst $ runRandState (approxPi (read num)) gen
        
