-- Write your parser in this file.

module Lab6  where

import           Control.Applicative          hiding (many)
import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Ord
import           Text.ParserCombinators.ReadP

type Name   = String  -- Variable names are strings.
type Number = Int     -- The kind of number in our language.

data MathExp
    = Number Number
    | Var    Name
    | Neg    MathExp
    | Plus   MathExp MathExp
    | Minus  MathExp MathExp
    | Mult   MathExp MathExp
    | Div    MathExp MathExp
    | Pow    MathExp MathExp
    | Greater MathExp MathExp
    | Lesser MathExp MathExp
    | Equalto MathExp MathExp
    | GorE   MathExp  MathExp
    | LorE  MathExp  MathExp
    | NotE  MathExp MathExp
    | Not MathExp
    | LetE   [Name] [MathExp] MathExp
    | IF    MathExp MathExp  MathExp
    | Fun Name MathExp MathExp
    deriving (Eq, Show)





parseIF :: ReadP MathExp
parseIF = do
    stringspace "if"
    cond     <- (parens (chainl1  parseMathE opchoice)) <++ (chainl1  parseMathE opchoice)
    stringspace "then"
    trueexp <- parseMathE
    stringspace "else"
    falseexp <- parseMathE 
    return $ IF cond trueexp falseexp

opchoice :: ReadP (MathExp -> MathExp -> MathExp)
opchoice = ( parseGreater <++ parseLesser <++ parseEqual <++ parseGorE <++ parseLorE <++ parseNotEqual)

pfunApp :: ReadP MathExp
pfunApp = do
    open <- stringspace "(\\"
    x <- parseString
    stringspace "->"
    mexp <- parseMathE
    close <- stringspace ")"
    arg <- parseMathE
    return $ Fun x mexp arg



parseGreater :: ReadP (MathExp -> MathExp -> MathExp)
parseGreater = do
    charspace '>'
    return Greater

parseLesser :: ReadP (MathExp -> MathExp -> MathExp)
parseLesser = do
    charspace '<'
    return Lesser

parseEqual :: ReadP (MathExp -> MathExp -> MathExp)
parseEqual = do
    stringspace "=="
    return Equalto

parseGorE :: ReadP (MathExp -> MathExp -> MathExp)
parseGorE = do
    stringspace ">="
    return GorE

parseLorE :: ReadP (MathExp -> MathExp -> MathExp)
parseLorE = do
    stringspace "<="
    return Equalto

parseNotEqual :: ReadP (MathExp -> MathExp -> MathExp)
parseNotEqual = do
    stringspace "/="
    return NotE

parseNotkey :: ReadP (MathExp -> MathExp)
parseNotkey = do
    stringspace "not"
    return Not
    
    
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
    charspace '+'
    return Plus

parseMult :: ReadP (MathExp -> MathExp -> MathExp)
parseMult = do
    charspace '*'
    return Mult

parseDiv :: ReadP (MathExp -> MathExp -> MathExp)
parseDiv = do
    charspace '/'
    return Div

parseMinus :: ReadP (MathExp -> MathExp -> MathExp)
parseMinus = do
    charspace '-'
    return Minus

parseNeg :: ReadP (MathExp -> MathExp)
parseNeg = do
    charspace '-'
    return Neg 

parsePow :: ReadP (MathExp -> MathExp -> MathExp)
parsePow = do
    charspace '^'
    return Pow




parseMathE2 :: ReadP MathExp
parseMathE2 = parseLetE <++ parseIF <++ mathparsetest 


parseMathE :: ReadP MathExp
parseMathE = notf <++ (chainr2 parseMathE2 opchoice)


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

parseParens :: ReadP MathExp                 
parseParens = parens parseMathE

parseNVP :: ReadP MathExp
parseNVP = (parseNeg <*> (parseNumber <++ parseVar <++ parseParens))
             <++ (parseNumber <++ parseParens <++ parseLetE <++ parseIF  <++ parseVar )

mathparsetest :: ReadP MathExp
mathparsetest = chainl2 (chainl2 ((parseNeg <*> chainr2 parseNVP parsePow) 
                <++ (chainr2 parseNVP parsePow))
                 (parseMult <++ parseDiv)) (parsePlus <++ parseMinus)
 

parseLetE :: ReadP MathExp
parseLetE = do
    stringspace "let"
    x <- (parens (sepwithspaces parseString (char ','))) <++ sepwithspaces parseString (char ',')
    stringspace "="
    mexp <- (parens (sepwithspaces parseMathE (char ','))) <++ sepwithspaces parseMathE (char ',')
    stringspace "in"
    vexp <- parseMathE  
    return $ LetE x  mexp vexp

parseString :: ReadP Name
parseString = do
    skipSpaces
    x <- satisfy (isLower)
    xs  <- munch (isAlphaNum)
    return (x:xs)

stringspace :: String -> ReadP String
stringspace str = skipSpaces >> string (str)  

charspace :: Char -> ReadP Char
charspace ch = skipSpaces >> char (ch)

parseTLE :: ReadP MathExp
parseTLE = do
    tle <- parseMathE
    skipSpaces
    return tle

sepwithspaces :: ReadP a -> ReadP sep -> ReadP [a]
sepwithspaces p1 s1 = sepBy1 p1 (skipSpaces >> s1)

parens :: ReadP a -> ReadP a
parens = between (charspace '(') (charspace ')') 

neg :: ReadP MathExp
neg = parseNeg >>= ( <$> choice0)

notf :: ReadP MathExp
notf = parseNotkey >>= ( <$> parseRel) where 
    parseRel = chainr2 parseMathE2 opchoice

choice0 :: ReadP MathExp
choice0 = parseLetE <++ parseIF <++ neg <++ choicem [parseNumber,parseVar, (parens mathparsetest)]

choice1 :: ReadP (MathExp -> MathExp -> MathExp)
choice1 = choicem [parsePlus,parseMinus]

choice2 :: ReadP (MathExp -> MathExp -> MathExp)
choice2 = choicem [parseDiv,parseMult]

choice3 :: ReadP (MathExp -> MathExp -> MathExp)
choice3 = parsePow 

choicem :: [ReadP a] -> ReadP a
choicem []     = pfail
choicem [p]    = p
choicem (p:ps) = p <++ choice ps

--dont change below this 
parse :: String -> Either String MathExp
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




