module Tests where

import Lab3

data TestResult
    = Success
    | Failure String

isSuccess :: TestResult -> Bool
isSuccess Success = True
isSuccess _       = False

message :: TestResult -> String
message Success           = "Success!"
message (Failure message) = message


expect1 :: (Show a, Show b, Eq b) => String -> (a -> b) -> a -> b -> TestResult
expect1 funcName func input expectedOutput =
    if expectedOutput == actual then
        Success
    else
        Failure $
            "Expected " ++ evaledStr ++
            " to be " ++ show expectedOutput ++
            ", but got " ++ show actual
    where
        actual    = func input
        evaledStr = funcName ++ " " ++ show input


tests :: [TestResult]
tests =
    [ expect1 "eval" eval
        (Number (-3) (1))
        (Fraction (-3) 1)
    , expect1 "eval" eval
        (Mult (Plus (Number 2 1) (Number (-6) 1)) (Plus (Number 3 1) (Number 2 1)))
        (Fraction (-20) 1)
    , expect1 "eval" eval
        (Div (Number 13 1) (Number 6 1))
        (Fraction 13 6)
    , expect1 "eval" eval
        (Plus (Mult (Number 2 1) (Number 3 1)) (Div (Number 13 1) (Number (-6) 1)))
        (Fraction (-23) (-6))
    , expect1 "tokenize" tokenize
        "1+2"
        [NumTok "1 / 1",AddTok,NumTok "2 / 1"]
    , expect1 "tokenize" tokenize
        "1 + 20"
        [NumTok "1 / 1",AddTok,NumTok "20 / 1"]
    , expect1 "tokenize" tokenize
        "1 * -2"
        [NumTok "1 / 1",MulTok,NumTok "-2 / 1"]
    , expect1 "tokenize" tokenize
        "(1 * 2 + 3)"
        [ParTok [NumTok "1 / 1",MulTok,NumTok "2 / 1",AddTok,NumTok "3 / 1"]]
    , expect1 "parse" parse 
        [NumTok "1 / 1",MulTok,NumTok "-2 / 1"]
        (Number (-2) 1)
    , expect1 "parse" parse
        [NumTok "1 / 1",AddTok,NumTok "20 / 1"]
         (Plus (Number 1 1) (Number 20 1))
    , expect1 "parse" parse 
        [NumTok "1 / 1",AddTok,NumTok "2 / 1"]
        (Plus (Number 1 1) (Number 2 1))
    ]



successes       = filter isSuccess tests
failures        = filter (not . isSuccess) tests
failureMessages = map message failures

results =
    ( length successes
    , length failures
    , failureMessages
    )

showFailures :: IO ()
showFailures = mapM_ putStrLn failureMessages
