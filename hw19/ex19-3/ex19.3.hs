import Data.Traversable
import Data.Foldable

addLabels :: (Traversable t) => t a -> t (Int,a)
addLabels tree = snd $ mapAccumL (\a b -> (a+1,(a,b))) 1 tree 

instance Traversable BinaryTree where
    traverse _ Empty = pure Empty 
    traverse f (Node left a right) =
         Node <$> traverse f left <*> f a <*> traverse f right

instance Functor BinaryTree where
        fmap _ Empty = Empty
        fmap f (Node left a right) =
            Node (fmap f left) (f a) (fmap f right)   

instance Foldable BinaryTree where
        foldr f acc Empty = acc
        foldr f acc (Node left a right) =
            foldr f (f a (foldr f acc right)) left


data BinaryTree a
        = Empty
        | Node (BinaryTree a) a (BinaryTree a)
        deriving Show

tree1 = Node (Node Empty 1 (Node Empty 2 Empty)) 3 (Node Empty 4 (Node Empty 5 Empty))

tree2 = Node (Node Empty 'a' (Node Empty 'b' Empty)) 'c' (Node Empty 'd' (Node Empty 'e' Empty))