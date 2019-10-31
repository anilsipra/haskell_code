 module Lab3 where

import Prelude hiding ((<+>),(<*>))
import Data.List
import Debug.Trace
import Data.Char
import Data.Maybe

data Token
    = NumTok String
    | AddTok
    | MulTok
    | DivTok
    | ParTok [Token]
    deriving (Show,Eq)


data ArithExp
    = Number Int Int
    | Plus ArithExp ArithExp
    | Mult ArithExp ArithExp
    | Div  ArithExp ArithExp
    deriving (Show,Eq)


eval :: ArithExp -> Fraction
eval (Number a b) = Fraction a b
eval (Plus a b) = eval a <+> eval b
eval (Mult a b) = eval a <*> eval b
eval (Div a b) =  eval a <*> (reciprocal (eval b))


tokenize :: String -> [Token]
tokenize (' ':rest) = tokenize rest
tokenize ('+':rest) = AddTok : tokenize rest
tokenize ('*':rest) = MulTok : tokenize rest
tokenize ('/':rest) = DivTok : tokenize rest
tokenize ('(':rest) = ParTok (tokenize (take n rest)) : tokenize (drop (n+1) rest) where
    n = fromJust (find p (elemIndices ')' rest)) where 
        p :: Int->Bool
        p a = (count ')' (take a rest)) == (count '(' (take a rest)) where
            count char list = length . (filter ( == char )) $ list       
tokenize ('-':rest) = NumTok (("-" ++ (takeWhile (isDigit) rest)) ++ " / 1") 
                         : tokenize (dropWhile (isDigit) rest)
tokenize (a:rest)   = if isDigit a then 
                         NumTok (([a] ++ takeWhile (isDigit) rest) ++ " / 1") 
                         : tokenize (dropWhile (isDigit) rest)
                      else []
tokenize []         = []  


parse :: [Token] -> ArithExp
parse [NumTok num]      = convertToNum num
parse [ParTok list]     = parse list
parse (a:MulTok:b:rest) = parse (NumTok (show (eval (Mult (parse [a]) (parse [b])))) : rest)
parse (a:DivTok:b:rest) = parse (NumTok (show (eval (Div  (parse [a]) (parse [b])))) : rest)
parse (a:AddTok:rest)   = Plus (parse [a]) (parse rest)


data Fraction = Fraction Int Int
                deriving (Eq)

(<+>) :: Fraction -> Fraction -> Fraction
(<+>) (Fraction a b) (Fraction c d) = Fraction (a*d + b*c) (b*d)

(<*>):: Fraction -> Fraction -> Fraction
(<*>) (Fraction a b) (Fraction c d) = Fraction (a*c) (b*d)

reciprocal :: Fraction -> Fraction
reciprocal (Fraction a b) = (Fraction b a)

reduce :: Fraction -> Fraction
reduce (Fraction a b) = Fraction (quot a (gcd a b)) (quot b (gcd a b))

instance Show Fraction where
    show (Fraction a b) = show a ++ " / " ++ show b


convertToNum :: String -> ArithExp
convertToNum s =  
                Number (read (takeWhile (/= ' ') s):: Int) (read ((drop 3) . (dropWhile (/=' ')) $ s) :: Int)




