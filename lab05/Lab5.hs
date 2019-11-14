import RandState
import System.Environment
import System.IO
import System.Random
import Control.Monad 
import Data.List    

data CardValue
    = King
    | Queen
    | Jack
    | NumberCard Int  
    deriving (Show, Eq)

data CardSuit
    = Hearts
    | Diamonds
    | Spades
    | Clubs
    deriving (Show, Eq)

data PlayingCard =
    PlayingCard CardValue CardSuit
    deriving (Eq)

type Deck = [PlayingCard]


instance Show PlayingCard where
    show (PlayingCard value suit) =
        valueStr value ++ suitStr suit
        where
            suitStr Hearts   = "H"  
            suitStr Diamonds = "D"
            suitStr Spades   = "S"
            suitStr Clubs    = "C"
            valueStr King           = "K"
            valueStr Queen          = "Q"
            valueStr Jack           = "J"
            valueStr (NumberCard n) = show n


fullCardDeck :: Deck
fullCardDeck =
    [ PlayingCard v s | v <- allVals, s <- allSuits ]
    where
        allVals  = King : Queen : Jack : [ NumberCard i | i <- [1..10] ]
        allSuits = [Hearts, Diamonds, Spades, Clubs]

randR :: Random a => (a, a) -> RandState a
randR (l,h) = do
    gen <- get
    let (x, gen') = randomR (l,h) gen
    put gen'
    return x

rollTwoDice :: RandState Int
rollTwoDice = do
    roll1 <- randR (1,6)
    roll2 <- randR (1,6)
    return (roll1 + roll2)

removeCard :: [PlayingCard] -> RandState (PlayingCard,[PlayingCard])
removeCard pack = do 
    pos <- randR (0,(length pack) -1)
    return (pack !! pos, take pos pack ++ drop (pos+1) pack)

shuffleDeck :: [PlayingCard] -> RandState [PlayingCard]
shuffleDeck pack = case pack of 
    [] -> return []
    pack -> do
        (card,deck) <- removeCard pack
        ((card:) <$> (shuffleDeck deck))

shuffleADeck :: RandState String
shuffleADeck  = do
    shuffledpack <- shuffleDeck fullCardDeck
    return (show shuffledpack)

shuffleNTimes :: Int -> StdGen -> IO ()
shuffleNTimes nTimes gen = 
    putStr . intercalate ['\n'] . fst $ (runRandState (replicateM nTimes shuffleADeck) gen)
    

rollTwoDiceNTimes :: Int -> StdGen -> IO ()
rollTwoDiceNTimes nTimes gen =
    putStr . intercalate ['\n'] . map show . fst $ (runRandState (replicateM nTimes rollTwoDice) gen)


usage :: String
usage =
    "Lab 5: Randomizer\n" ++
    "\n" ++
    "$ ./Lab5 shuffle 600      # 600 times: output a full deck shuffle\n" ++
    "$ ./Lab5 rollTwoDice 800  # 800 times: output the sum of rolling two dice\n" ++
    "\n"

main :: IO ()
main = do
    gen  <- newStdGen
    args <- getArgs
    case args of
        ["shuffle",     nTimes] -> shuffleNTimes     (read nTimes) gen
        ["rollTwoDice", nTimes] -> rollTwoDiceNTimes (read nTimes) gen
        _                       -> putStrLn usage
