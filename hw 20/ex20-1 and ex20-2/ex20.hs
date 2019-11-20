import Control.Monad hiding (Trans.Maybe)    
import Control.Applicative

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)} 

--ex20.1
instance Monad m => Functor (MaybeT m) where
        fmap f (MaybeT mma) = MaybeT $ do
           ma <- mma
           pure $ fmap f ma 
 
--ex20.2           
instance Monad m => Applicative (MaybeT m) where
   pure = MaybeT . pure . Just 
   (MaybeT mmf) <*> (MaybeT mma) = MaybeT $ do
        mf <- mmf
        ma <- mma
        pure $ mf <*> ma

--for testing
v1 = MaybeT ([Just 3])
v2 = MaybeT ([Just 2])
v3 = MaybeT ([Nothing])
f  = MaybeT ([Just (+)])

