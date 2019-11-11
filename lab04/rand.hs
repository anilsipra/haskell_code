rollTwoDice :: RandomState Int
rollTwoDice = do
    roll1 <- randR (1,6)
    roll2 <- randR (1,6)
    return $ roll1 + roll2

removeCard :: RandState (PlayingCard,[PlayingCard])
removeCard = do
    pack <- snd . get
    num <- randR (1,6)
    return (pack !! num,take num pack ++ drop num+1 pack)

shuffleDeck :: RandState [PlayingCard]
shuffleDeck [] = return []
shuffleDeck pack = removeCard shuffle rest and add removed card on top 

shuffleNTimes n = replicateM n shuffleDeck

