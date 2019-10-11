{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module AbstractInteger where

-- Here are some definations for AbstractNatural.
-- You will probably define your AbstractInteger based on
-- AbstractNatural.

data AbstractNatural = Zero | S AbstractNatural
    deriving (Show)

-- Once we tell Haskell that AbstractNatural can do equality
-- comparisons and how AbstractNatural is totally ordered, we
-- get other functions for free, like /= and >= and > and <
--
-- You may not need these so I've left them commented out, but
-- you should understand why they work.
-- 
-- instance Eq AbstractNatural where
--   Zero == Zero = True
--   Zero == S _  = False
--   S _  == Zero = False
--   S x  == S y  = x == y
--
-- instance Ord AbstractNatural where
--   Zero <= Zero = True
--   Zero <= S _  = True
--   S _  <= Zero = False
--   S x  <= S y  = x <= y
--
-- successorNat :: AbstractNatural -> AbstractNatural
-- successorNat = S
--
-- predecessorNat :: AbstractNatural -> AbstractNatural
-- predecessorNat Zero  = Zero
-- predecessorNat (S x) = x


-- Figure out how you will define integers...

data AbstractInteger = Pos AbstractNatural | Neg AbstractNatural
    deriving (Show)

-- ...then fill out the functions below for your AbstractInteger type.

successor :: AbstractInteger -> AbstractInteger
successor (Pos x)= Pos (S x)
successor (Neg Zero)= Pos (S Zero)
successor (Neg (S x))= Neg x


predecessor :: AbstractInteger -> AbstractInteger
predecessor (Neg x) = Neg (S x)
predecessor (Pos Zero)= Neg (S Zero)
predecessor (Pos (S x))= Pos x 

-- Be sure to add type declarations to all these functions too.
negator :: AbstractInteger->AbstractInteger
negator (Pos x)   = Neg x
negator (Neg x)   = Pos x

absolute :: AbstractInteger->AbstractInteger
absolute (Pos x) = Pos x
absolute (Neg x) = Pos x

add :: AbstractInteger->AbstractInteger->AbstractInteger
add (Pos x) (Pos Zero) = Pos x
add (Pos x) (Neg Zero) = Pos x 
add (Pos x) (Pos (S y)) = successor (add (Pos x) (Pos y))
add (Neg x) (Pos Zero)= Neg x
add (Neg x) (Neg Zero)= Neg x
add (Neg x) (Neg (S y))= negator (add (Pos x) (Pos (S y)))
add (Pos x) (Neg (S y)) = predecessor (add (Pos x) (Neg y))
add a b= add b a

difference :: AbstractInteger->AbstractInteger->AbstractInteger
difference a b = add a (negator b)

multiply :: AbstractInteger->AbstractInteger->AbstractInteger
multiply (Pos Zero) (Pos x)= Pos Zero
multiply (Pos Zero) (Neg x)=Pos Zero
multiply (Neg Zero) (Pos x)= Pos Zero
multiply (Neg Zero) (Neg x)= Pos Zero
multiply (Pos (S x)) (Pos y)= add (multiply (Pos x) (Pos y)) (Pos y)
multiply (Pos (S x)) (Neg y)= negator (multiply (Pos (S x)) (Pos y))
multiply (Neg x) (Neg y)= multiply (Pos x) (Pos y)
multiply a b= multiply b a

-- To define division and modulo, you will probably need
-- comparison functions: == <= < > >=.
--
-- If you just provide == and <= below, Haskell will give
-- you the rest for free.
instance Eq AbstractInteger where
    x == y = case (difference x y) of 
        Pos Zero -> True
        Neg Zero -> True
        _ -> False
    -- add more cases here...

instance Ord AbstractInteger where
    x <= y = case (difference x y) of 
        Pos Zero -> True
        Neg _    -> True
        Pos _ -> False
    -- add more cases here...

divide :: AbstractInteger->AbstractInteger->AbstractInteger
divide (Pos a) (Pos b) 
        | (Pos a) < (Pos b) = Pos Zero
        | (Pos a) == (Pos b) = Pos (S Zero)
        | otherwise = successor (divide (difference (Pos a) (Pos b)) (Pos b))
divide (Neg a) (Neg b)= negator (divide (Neg a) (Pos b))
divide (Pos a) (Neg b)= negator (divide (Pos a) (Pos b))
divide (Neg a) (Pos b)= predecessor (divide (add (Neg a) (Pos b)) (Pos b))

modulo :: AbstractInteger->AbstractInteger->AbstractInteger
modulo a b = difference a (multiply b (divide a b))

toAbstract :: Integer -> AbstractInteger
toAbstract 0 = Pos Zero
toAbstract x
           | x>0 = successor $toAbstract (x-1)
           | otherwise = negator (toAbstract (-x))


fromAbstract :: AbstractInteger -> Integer
fromAbstract (Pos Zero) = 0
fromAbstract (Neg Zero) = 0
fromAbstract (Pos (S x))= 1 + fromAbstract (Pos x)
fromAbstract (Neg (S x))= fromAbstract (Neg x) - 1 

-- Take a list of strings, calculate, and return a string result.
-- You should not need to modify this, but you may eta-reduce it if you like.
evaluateRPN :: [String] -> AbstractInteger
evaluateRPN inputList = evalRPNStack [] inputList

-- The core of the RPN caluculator, Stack -> InputList -> Output
-- You will need to provide more cases.
evalRPNStack :: [AbstractInteger] -> [String] -> AbstractInteger
evalRPNStack stack inputList =
    case (stack, inputList) of
        ( x:_,           [] )            -> x -- No more input, return top of stack.
        ( y:x:stackRest, "+":inputRest ) -> evalRPNStack (add x y        : stackRest) inputRest
        ( y:x:stackRest, "*":inputRest ) -> evalRPNStack (multiply x y   : stackRest) inputRest
        ( y:x:stackRest, "-":inputRest ) -> evalRPNStack (difference x y : stackRest) inputRest
        ( y:x:stackRest, "/":inputRest ) -> evalRPNStack (divide x y     : stackRest) inputRest
        ( y:x:stackRest, "%":inputRest ) -> evalRPNStack (modulo x y     : stackRest) inputRest
        ( x:stackRest, "abs":inputRest ) -> evalRPNStack (absolute x     : stackRest) inputRest
        -- ...add more cases here...
        -- This last case handles numeric inputs, "0" "-2" "34" etc...
        ( _,          numStr:inputRest ) -> evalRPNStack (toAbstract (read numStr) : stack) inputRest

-- Convenience constructors. Handy for testing in ghci.
-- Define zero after you've written your definition of AbstractInteger.
-- Once you define zero you should get the rest for free.
zero  = Pos Zero
one   = successor zero
two   = successor one
three = successor two
four  = successor three
five  = successor four
six   = successor five
seven = successor six
eight = successor seven
nine  = successor eight
ten   = successor nine

negativeOne   = predecessor zero
negativeTwo   = predecessor negativeOne
negativeThree = predecessor negativeTwo
negativeFour  = predecessor negativeThree
negativeFive  = predecessor negativeFour
negativeSix   = predecessor negativeFive
negativeSeven = predecessor negativeSix
negativeEight = predecessor negativeSeven
negativeNine  = predecessor negativeEight
negativeTen   = predecessor negativeNine
