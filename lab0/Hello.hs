data Level
    = Undergraduate
    | Masters
    | PhD
    | Other
    deriving Show


name :: String
name = "Rachit Surana"

level :: Level
level = Undergraduate

major :: String
major = "Computer Science+Math+Econ"

why :: String
why = "To develop skill in programming and learn haskell which is not a common asset."

distance :: Int -> Int -> Int
distance rate time = rate * time
main :: IO ()
main = do
    putStrLn "Hello, world!"
    putStrLn ""
    putStrLn ("My name is " ++ name ++ ".")
    putStrLn ("I am a " ++ show level ++ " student.")
    putStrLn ("I'm going into " ++ major ++ ".")
    putStrLn ("I'm in this class because " ++ why)
    putStrLn ""
    putStrLn ("If you travel for 15mph for 30 hours you will go " ++ show (distance 15 30) ++ " miles.")
