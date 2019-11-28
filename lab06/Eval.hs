module Eval where

import           Control.Applicative
import           Lab6


data Result 
        = B Bool 
        | Ans Number
        deriving (Eq,Show)

-- Error message or result number.
type EvalResult = Either String Result

type Bindings = [(Name, MathExp)]


allRight :: [EvalResult] -> Either String [Result]
allRight = foldr (liftA2 (:)) (Right [])

eval :: MathExp -> EvalResult
eval (LetE names mathExps mainExp)
    | length names == length mathExps = performEval
    | otherwise                       = Left errorMsg
    where
        performEval = do
            let bindings = bindingsOrError
            evalMathExp bindings mainExp
        bindingsOrError =
            zip names (mathExps)
        errorMsg =
            "must assign " ++ show (length names) ++
            " names but given " ++ show (length mathExps) ++
            " expressions"
eval (IF cond trueexp falseexp) = case (evalMathExp [] cond) of 
    Right (B True) -> eval trueexp
    Right  (B False) -> eval falseexp
    Left str -> Left $ str ++ "here"
eval mathExp = evalMathExp [] mathExp


evalMathExp :: Bindings -> MathExp -> EvalResult
evalMathExp bi mexp = 
    let 
        recurse e = evalMathExp bi e
        ansfb f (Ans n1) (Ans n2) = Ans (f n1 n2)
        ansfu f (Ans n) = Ans $ f n
        ansfb2 f (Ans n1) (Ans n2) = B (f n1 n2)
        unOp op e      = (ansfu op) <$> recurse e
        binOp op e1 e2 = liftA2 (ansfb op) (recurse e1) (recurse e2)
        invert (B b) = B $ not b
        checknum a = case  (recurse a) of
            Right (Ans _) -> True
            otherwise -> False
        checkbool a = case (recurse a) of 
            Right (B _) -> True
            otherwise -> False
        check1 e1 e2 =(checknum e1) && (checknum e2)
    in
    case mexp of 
        Number n -> Right $ Ans n
        Var n    -> case lookup n bi of 
            Just e -> evalMathExp bi e
            Nothing -> Left $ "could not find variable \"" ++ n ++ "\""
        Plus e1 e2 -> if (check1 e1 e2) 
                      then binOp (+) e1 e2 
                      else Left $ "type mismatch1"
        Neg   e     -> if checknum e 
                       then unOp negate e
                       else Left $ "type mismatch2"
        Minus e1 e2 -> if (check1 e1 e2) 
                       then binOp subtract e2 e1
                       else Left $ "type mismatch3"
        Mult e1 e2 -> if (check1 e1 e2) 
                      then binOp (*) e1 e2 
                      else Left $ "type mismatch4"
        Div e1 e2 -> if (not $ check1 e1 e2) then Left ("type mismatch")   
                     else if recurse e2 == Right (Ans 0) 
                          then Left $ "Division by zero"
                          else binOp quot e1 e2
        Pow   e1 e2 -> if (not $ check1 e1 e2) then Left ("type mismatch")
                       else case recurse e2 of
                        Right (Ans pow) -> if pow < 0
                             then Left "negative exponent"
                             else binOp (^) e1 e2
                        Left err  -> Left err
        Greater e1 e2 -> if (check1 e1 e2) then liftA2 (ansfb2 (>)) (recurse e1) (recurse e2)
                         else Left $ "type mismatch5"
        Lesser e1 e2 -> if (check1 e1 e2) 
                        then liftA2 (ansfb2 (<)) (evalMathExp bi e1) (evalMathExp bi e2)
                        else Left $ "type mismatch6"
        Equalto e1 e2 -> if (check1 e1 e2) 
                        then liftA2 (ansfb2 (==)) (evalMathExp bi e1) (evalMathExp bi e2)
                         else Left $ "type mismatch7"
        GorE e1 e2 -> if (check1 e1 e2) 
                      then liftA2 (ansfb2 (>=)) (evalMathExp bi e1) (evalMathExp bi e2)
                      else Left $ "type mismatch8"
        LorE e1 e2 -> if (check1 e1 e2) 
                      then liftA2 (ansfb2 (<=)) (evalMathExp bi e1) (evalMathExp bi e2)
                      else Left $ "type mismatch9"
        NotE e1 e2 -> if (check1 e1 e2) 
                      then liftA2 (ansfb2 (/=)) (evalMathExp bi e1) (evalMathExp bi e2)
                      else Left $ "type mismatch0"
        Not e ->  if checkbool (e) then invert <$> (evalMathExp bi e) else Left $"type mismatch11"
        LetE names exps maine -> evalMathExp (bchanged) maine where
            bchanged =  zip (fst (unzip bi) ++ names)  (snd (unzip bi) ++ exps)
        IF cond truee falsee -> case recurse cond of 
            Right (B True) -> recurse truee
            Right (B False) -> recurse falsee  
            Left err -> Left err


