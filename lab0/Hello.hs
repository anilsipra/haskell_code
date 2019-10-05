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