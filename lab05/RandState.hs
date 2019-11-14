module RandState where

import System.Random

newtype RandState a = RandState {
  runRandState :: StdGen -> (a, StdGen)
}

instance Functor RandState where
  fmap f ra = RandState $ \s -> 
    let (a,g) = runRandState ra s
    in (f a,g)

instance Applicative RandState where
  pure a= RandState $ \s -> (a,s)
  rf <*> ra = RandState $ \s ->
    let (f,g) = runRandState rf s
        (a,g') = runRandState ra g
    in (f a,g')

instance Monad RandState where
  ra >>= f = RandState $ \s -> 
    let (a,g) = runRandState ra s
        rb = f a
        (b,g') = runRandState rb g
    in (b,g')

get :: RandState StdGen
get = RandState $ \gen -> (gen, gen)

put :: StdGen -> RandState ()
put gen' = RandState $ \gen -> ((), gen')

runRandom :: RandState a -> StdGen -> a
runRandom (RandState f) s = fst $ f s

rand :: Random a => RandState a
rand = do
    gen <- get
    let (x, gen') = random gen
    put gen'
    return x
