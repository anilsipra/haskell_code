-- Unparse expressions for debug display.
--
-- You should not need to modify this for the regular lab.
--
module Unparse (
  unparse
) where

import           Data.List
import           Lab6

unparse :: MathExp -> String
unparse (LetE names assigns main) =
    "let " ++ commaList names ++ " = " ++
    (commaList . map unparse $ assigns) ++ " in " ++
    unparse main
    where
        commaList [str] =  str
        commaList strs  = "(" ++ intercalate ", " strs ++ ")"
unparse (IF _ _ _ ) = ""
unparse (Fun _ _ _ ) = ""
unparse (mathExp)           = unparseMathExp mathExp


unparseMathExp :: MathExp -> String
unparseMathExp (Number n)     = show n
unparseMathExp (Var    name)  = name
unparseMathExp (Neg    e1)    = "-" ++ unparseMathExp e1
unparseMathExp (Plus   e1 e2) = "(" ++ unparseMathExp e1 ++ " + " ++ unparseMathExp e2 ++ ")"
unparseMathExp (Minus  e1 e2) = "(" ++ unparseMathExp e1 ++ " - " ++ unparseMathExp e2 ++ ")"
unparseMathExp (Mult   e1 e2) = "(" ++ unparseMathExp e1 ++ " * " ++ unparseMathExp e2 ++ ")"
unparseMathExp (Div    e1 e2) = "(" ++ unparseMathExp e1 ++ " / " ++ unparseMathExp e2 ++ ")"
unparseMathExp (Pow    e1 e2) = "(" ++ unparseMathExp e1 ++  "^"  ++ unparseMathExp e2 ++ ")"
unparseMathExp (Greater e1 e2) = "(" ++ unparseMathExp e1 ++  ">"  ++ unparseMathExp e2 ++ ")"
unparseMathExp (Lesser e1 e2) = "(" ++ unparseMathExp e1 ++  "<"  ++ unparseMathExp e2 ++ ")"
unparseMathExp (GorE e1 e2) = "(" ++ unparseMathExp e1 ++  ">="  ++ unparseMathExp e2 ++ ")"
unparseMathExp (LorE e1 e2) = "(" ++ unparseMathExp e1 ++  "<="  ++ unparseMathExp e2 ++ ")"
unparseMathExp (Equalto e1 e2) = "(" ++ unparseMathExp e1 ++  "=="  ++ unparseMathExp e2 ++ ")"
unparseMathExp (NotE e1 e2) = "(" ++ unparseMathExp e1 ++  "/="  ++ unparseMathExp e2 ++ ")"
unparseMathExp (Not e1 ) = "not" ++ unparseMathExp e1 
