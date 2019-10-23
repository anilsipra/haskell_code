 module Lab3 where

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
    = Number Int
    | Plus ArithExp ArithExp
    | Mult ArithExp ArithExp
    | Div  ArithExp ArithExp
    deriving (Show,Eq)


-- eval :: ArithExp -> Int
eval (Number a) = a
eval (Plus a b) = eval a + eval b
eval (Mult a b) = eval a * eval b
eval (Div a b) = quot (eval a) (eval b)





-- tokenize :: String -> [Token]
tokenize (' ':rest) = tokenize rest
tokenize ('+':rest) = AddTok : tokenize rest
tokenize ('*':rest) = MulTok : tokenize rest
tokenize ('/':rest) = DivTok : tokenize rest

 --   ParTok (tokenize (takeWhile (/=')') rest)) : tokenize (drop 1 (dropWhile (/=')') rest)) 
   
tokenize ('(':rest) = ParTok (tokenize (take n rest)) : tokenize (drop (n+1) rest) where
    n = fromJust (find p (elemIndices ')' rest)) where 
        p :: Int->Bool
        p a = (length . (filter (==')')) $ (take a rest)) == (length . (filter (=='(')) $ (take a rest))       
tokenize ('-':rest) = NumTok ("-" ++ (takeWhile (isDigit) rest)) : tokenize (dropWhile (isDigit) rest)
tokenize (a:rest) = if isDigit a then NumTok ([a] ++ takeWhile (isDigit) rest) : tokenize (dropWhile (isDigit) rest)
                            else []
tokenize [] = []  



-- The trick to parsing is: what do you do last?
-- parse :: [Token] -> ArithExp

parse [NumTok num] = Number (read num :: Int)
parse [ParTok list] = parse list
parse (a:MulTok:b:rest) = parse (NumTok (show (eval (Mult (parse [a]) (parse [b])))) : rest)
parse (a:DivTok:b:rest) = parse (NumTok (show (eval (Div  (parse [a]) (parse [b])))) : rest)
parse (a:AddTok:rest)  = Plus (parse [a]) (parse rest)
    --parse (NumTok (show (eval (Plus  (parse [a]) (parse [b])))) : rest)
