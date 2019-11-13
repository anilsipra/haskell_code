module Parser where

    import Control.Monad.State
    import Control.Applicative
    import Data.Char
    
    type Parser = StateT String []
    
    runParser :: Parser a -> String -> [(a,String)]
    runParser = runStateT
    
    parser :: (String -> [(a,String)]) -> Parser a
    parser = StateT
    
    satisfy :: (Char -> Bool) -> Parser Char
    satisfy f = parser $ \s -> case s of
        [] -> []
        a:as -> [(a,as) | f a]
    
    char :: Char -> Parser Char
    char = satisfy . (==)

    digit :: Parser Char
    digit = satisfy isDigit
    
    string :: String -> Parser String
    string = mapM char

    parseInt :: Parser Int
    parseInt = read <$> (some digit)

    convert :: Int -> ComplexInt 
    convert num = ComplexInt num 0

    data ComplexInt = ComplexInt Int Int
        deriving (Show)

    parseComplexSingle :: Parser ComplexInt
    parseComplexSingle = convert <$> parseInt

    parseComplexDouble :: Parser ComplexInt
    parseComplexDouble = do 
        char '('
        num1 <- parseInt
        char ','
        num2 <- parseInt 
        char ')'
        return (ComplexInt num1 num2)

    parseComplexInt = parseComplexSingle <|> parseComplexDouble

    instance Read ComplexInt where
        readsPrec _ = runParser parseComplexInt