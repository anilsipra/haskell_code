-- Write your parser in this file.

module Lab6 (
  Name,
  Number,
  TopLevelExp(..),
  MathExp(..),
  parse
) where

import           Control.Applicative          hiding (many)
import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Ord
import           Text.ParserCombinators.ReadP

type Name   = String  -- Variable names are strings.
type Number = Int     -- The kind of number in our language.


-- A top-level expression is either:
--
-- 1) A bare mathematical expression:
--
-- 4 + (2*5)
--
-- 2) A let-binding followed by an expression to evaluate:
--
-- let x = 5 in x + 4
--
-- let (x1, y1, x2, y2) = (5,5,10,10) in (y2-y1)*(y2-y1) + (x2-x1)*(x2-x1)
--
-- You can assume that the tuples on either side of the = sign are
-- always the same length--if not it will be treated as an eval error.

data TopLevelExp
    = MathTLE MathExp
    | LetTLE [Name] [MathExp] MathExp
    deriving (Eq, Show)


-- A math expression is a number, a variable,
-- a negation of a math expression, or any of
-- the four major operations plus power on a
-- pair of math expressions.
--
-- In the actual parser:
--   1. Precedence should be standard order of operations.
--   2. Negation should only precede a number, a variable, or
--      a parenthetical expression.
--   3. A variable starts with a lowercase letter and after
--      the first letter is any alphanumeric character a-z A-Z 0-9.
--
-- Your parser does _not_ need an explicit tokenization step like Lab 3.
-- In the functional parsing paradigm, tokenization+parsing occur
-- simultaneously.

data MathExp
    = Number Number
    | Var    Name
    | Neg    MathExp
    | Plus   MathExp MathExp
    | Minus  MathExp MathExp
    | Mult   MathExp MathExp
    | Div    MathExp MathExp
    | Pow    MathExp MathExp
    deriving (Eq, Show)




parseNumber :: ReadP MathExp
parseNumber = do
    skipSpaces
    Number <$> (read <$> munch1 (isDigit)) 

parseVar :: ReadP MathExp
parseVar = do
    skipSpaces
    x <- satisfy (isLower)
    xs  <- munch (isAlphaNum)
    return $ Var (x:xs)

parsePlus :: ReadP (MathExp -> MathExp -> MathExp)
parsePlus = do
    skipSpaces
    char '+'
    return Plus

parseMult :: ReadP (MathExp -> MathExp -> MathExp)
parseMult = do
    skipSpaces
    char '*'
    return Mult

parseDiv :: ReadP (MathExp -> MathExp -> MathExp)
parseDiv = do
    skipSpaces
    char '/'
    return Div

parseMinus :: ReadP (MathExp -> MathExp -> MathExp)
parseMinus = do
    skipSpaces
    char '-'
    return Minus

parseNeg :: ReadP (MathExp -> MathExp)
parseNeg = do
    skipSpaces
    char '-'
    return Neg 

parsePow :: ReadP (MathExp -> MathExp -> MathExp)
parsePow = do
    skipSpaces
    char '^'
    return Pow


parseMathTLE :: ReadP TopLevelExp
parseMathTLE = MathTLE <$> (mathparsetest)  

--mathparsetest = (chainl1 (chainl1 (chainr1 choice0 choice3) choice2) choice1)

chainl2 :: ReadP a -> ReadP (a -> a -> a) -> ReadP a
chainl2 p op = p >>= rest
  where rest x = do f <- op
                    y <- p
                    rest (f x y)
                <++ return x

chainr2 :: ReadP a -> ReadP (a -> a -> a) -> ReadP a
chainr2 p op = scan
  where scan   = p >>= rest
        rest x = do f <- op
                    y <- scan
                    return (f x y)
                 <++ return x

parseParens = parens mathparsetest

parseNVP = (parseNeg <*> (parseNumber <++ parseVar <++ parseParens)) <++ (parseNumber <++ parseVar <++ parseParens)

mathparsetest = chainl2 (chainl2 ((parseNeg <*> chainr2 parseNVP parsePow) <++ (chainr2 parseNVP parsePow)) (parseMult <++ parseDiv)) (parsePlus <++ parseMinus)


                

    --pfail 
 

parseLetTLE :: ReadP TopLevelExp
parseLetTLE = do
    stringspace "let"
    x <- (parens (sepwithspaces parseString (char ','))) <++ sepwithspaces parseString (char ',')
    stringspace "="
    mexp <- (parens (sepwithspaces mathparsetest (char ','))) <++ sepwithspaces mathparsetest (char ',')
    stringspace "in"
    vexp <- mathparsetest  
    return $ LetTLE x  mexp vexp

parseString :: ReadP Name
parseString = do
    skipSpaces
    x <- satisfy (isLower)
    xs  <- munch (isAlphaNum)
    return (x:xs)

stringspace str = skipSpaces >> string (str)    

parseTLE :: ReadP TopLevelExp
parseTLE = do
    tle <- parseLetTLE +++ parseMathTLE
    skipSpaces
    return tle

sepwithspaces p1 s1 = sepBy1 p1 (skipSpaces >> s1)
parens = between (skipSpaces >> char '(') (skipSpaces >> char ')') 
neg = skipSpaces >> parseNeg >>= ( <$> choice0) 
choice0 = neg <++ choicem [parseNumber,parseVar, (parens mathparsetest)]
choice1 = choicem [parsePlus,parseMinus]
choice2 = choicem [parseDiv,parseMult]
choice3 = parsePow 

choicem :: [ReadP a] -> ReadP a
-- ^ Combines all parsers in the specified list.
choicem []     = pfail
choicem [p]    = p
choicem (p:ps) = p <++ choice ps

-- Run the parser on a given string.
--
-- You should not modify this function. Grading may
-- look for the specific messages below.
parse :: String -> Either String TopLevelExp
parse str =
    case (completeParses, incompleteParses) of
        ([(result, "")], _  ) -> Right result  -- Only complete result.
        ([]            , [] ) -> Left $ "No parse."
        ([]            , _:_) -> Left $ "Incomplete parse. Unparsed: " ++ show leastRemaining
        (_:_           , _  ) -> Left $ "Ambiguous parse: " ++ show completeParses
    where
        parses = readP_to_S parseTLE str
        (completeParses, incompleteParses) =
            partition (\(_, remaining) -> remaining == "") parses
        leastRemaining = minimumBy (comparing length) . map snd $ incompleteParses




