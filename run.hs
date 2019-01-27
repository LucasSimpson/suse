import Data.Bifunctor
import Data.Maybe
import Data.Either

import System.IO  
  
-------------------- MAIN EXECUTION --------------------
-- read from file, parse, execute, show --
main = do  
    handle <- openFile "input.txt" ReadMode  
    contents <- hGetContents handle  
    putStr . (flip (++)) "\n" . pullInfo $ rp debug $ splitNL contents 
    hClose handle where
        pullInfo (Result (either, s)) = show either 
        pullInfo (failure) = show failure

-------------------- generic helper dat / funcs --------------------

maybeToEither :: String -> Maybe a -> Either String a
maybeToEither _ (Just x) = Right x
maybeToEither errMessage (Nothing) = Left errMessage

splitNL :: String -> [String]
splitNL [] = []
splitNL [x] = [[x]]
splitNL (x:text) = if (x == '\n') then (
        [[x]] ++ rest
    ) else (
        [(x:(head rest))] ++ (tail rest) 
    ) where
    rest = splitNL text 

brackets :: String -> String
brackets s = "(" ++ s ++ ")"

index :: Integer -> [s] -> Maybe s
index _ [] = Nothing
index 0 (x:xs) = Just x
index n xs 
    | n < 0 = Nothing
    | otherwise = index (n - 1) (tail xs)

grindex :: Integer -> Integer -> [[s]] -> Maybe s
grindex x y grid = do
    row <- index x grid
    index y row

mGetOrElse :: Maybe s -> s -> s
mGetOrElse (Just x) _ = x
mGetOrElse Nothing y = y

data Stack a = Stack [a] deriving Show

newStack :: Stack a
newStack = Stack []

push :: a -> Stack a -> Stack a
push x (Stack s) = Stack (x:s)

pop :: Stack a -> (Maybe a, Stack a)
pop (Stack []) = (Nothing, newStack)
pop (Stack (x:xs)) = (Just x, Stack xs)

peek :: Stack a -> Maybe a
peek (Stack []) = Nothing
peek (Stack (x:xs)) = Just x

p1 :: (a, b, c) -> a
p1 (x, y, z) = x
p2 :: (a, b, c) -> b
p2 (x, y, z) = y
p3 :: (a, b, c) -> c
p3 (x, y, z) = z

-------------------- Generic Parsers --------------------

type ParserLabel = String
type ParserError = String
type ParserPosition = (Integer, Integer, String)

data Result a = Result a | Failure (ParserLabel, ParserError, ParserPosition)

instance (Show a) => Show (Result a) where
    show (Result r) = "Result " ++ (show r)
    show (Failure (label, message, pos)) = "Error line " ++ (show . p1 $ pos) ++ ", row " ++ (show . p2 $ pos) ++ ": " ++ (p3 pos) ++ "\nFailure parsing " ++ label ++ ": " ++ message

instance Functor Result where

    fmap f (Result r) = Result $ f r
    fmap f (Failure err) = Failure err

instance Applicative Result where
    pure x = Result x
    (Result f) <*> (Result r2) = Result $ f r2
    _ <*> (Failure err) = Failure err
    (Failure err) <*> _ = Failure err

instance Monad Result where
    return = pure
    Result r >>= f = f r
    Failure err >>= f = Failure err

getOrElse :: Result a -> Result a -> Result a
getOrElse (Result r1) _ = Result r1
getOrElse (Failure err) r2 = r2


type Position = (Integer, Integer)

data InputState s = InputState { lines :: [[s]], position :: Position } deriving Show

data Parser s a = Parser { doParse :: InputState s -> Result (a, InputState s), label :: ParserLabel }

initPosition = (-1, 0)

col :: Position -> Integer
col = snd

line :: Position -> Integer
line = fst

incCol :: Position -> Position
incCol (l, c) = (l, c + 1)

incLine :: Position -> Position
incLine (l, c) = (l + 1, 0)

nextToken :: InputState s -> (Maybe s, InputState s)
nextToken (InputState lines position) = getOrElse result (Nothing, InputState lines position)
    where getOrElse (Just t, remInput) _ = (Just t, remInput)
          getOrElse (Nothing, remInput) x = x
          tryPos = \nextPos -> let
                        newToken = do
                            x <- index (line nextPos) lines
                            index (col nextPos) x
                        in (newToken, InputState lines nextPos)
          result = getOrElse (tryPos $ incCol position) (tryPos $ incLine position)
          
toParserPos :: (Show s) => InputState s -> ParserPosition
toParserPos (InputState lines position) = (fst position, snd position, mGetOrElse (fmap (foldl (++) "" . fmap show) (index (fst position) lines)) "EOF")

plabel :: Parser s a -> String
plabel (Parser f l) = l

pfunc :: Parser s a -> InputState s -> Result (a, InputState s)
pfunc (Parser f l) = f


instance Show (Parser s a) where
    show = plabel

instance Functor (Parser s) where
    fmap f (Parser pf pl) = Parser (fmap (first f) . pf) pl

instance Applicative (Parser s) where
    pure f = Parser (\text -> Result (f, text)) ""
    pa <*> pb = do
        f <- pa
        x <- pb
        pure (f x)

instance Monad (Parser s) where
    return = pure
    (Parser doParse pl) >>= f = Parser (\text -> 
        do
            r1 <- doParse text
            runParser (f . fst $ r1) $ snd r1) pl

runParser :: Parser s a -> InputState s -> Result (a, InputState s)
runParser (Parser doParse pl) inputState = doParse inputState

-- String shortcut --
rp :: Parser Char a -> [String] -> Result (a, InputState Char)
rp parser lines = runParser parser (toInput lines)
    where toInput = \lines -> InputState lines initPosition

setLabel :: String -> Parser s a -> Parser s a
setLabel newLabel parser = Parser (\text -> 
    case pfunc parser $ text of 
        (Result r) -> Result r
        (Failure (oldLabel, message, pos)) -> Failure (newLabel, message, pos)
    ) newLabel
( <%> ) = flip setLabel

satisfy :: (Show a) => (a -> Bool) -> String -> Parser a a
satisfy predicate label = Parser (\inputState -> let (mToken, remInput) = nextToken inputState
                                            in case mToken of 
        Nothing -> Failure (label, "EOF", (-1, -1, "temp"))
        Just t -> if predicate t then (
                Result (t, remInput)
            ) else (
                Failure (label, "Unexpected " ++ (show t), toParserPos remInput)
            )
    ) label
    

pfor :: (Eq a, Show a) => a -> Parser a a
pfor token = satisfy (\t -> t == token) (show token)

pchar :: Char -> Parser Char Char
pchar = pfor

pand :: Parser s a -> Parser s b -> Parser s (a, b)
pand pa pb = (do
    r1 <- pa
    r2 <- pb
    return (r1, r2)) <%> ((plabel pa) ++ (plabel pb))
( .>>. ) = pand

pandf :: Parser s a -> Parser s b -> Parser s a
pandf pa pb = fmap fst (pa .>>. pb)
( .>> ) = pandf

pands :: Parser s a -> Parser s b -> Parser s b
pands pa pb = fmap snd (pa .>>. pb)
( >>. ) = pands

por :: Parser s a -> Parser s a -> Parser s a
por (Parser parseA pla) (Parser parseB plb) = newParser <%> (brackets $ pla ++ "|" ++ plb)
    where newParser = Parser (\text -> getOrElse (parseA text) (parseB text)) ("")
( <|> ) = por

anyOf :: String -> Parser Char Char
anyOf letters = newParser <%> (brackets . tail . plabel $ newParser)
    where newParser = foldl (ored) (Parser (\s -> Failure ("", "", (-1, -1, ""))) "") . fmap pchar $ letters
          ored = \x y -> let p = x <|> y
                         in p <%> (init . tail . plabel $ p)

pseq :: (Eq s, Show s) => [s] -> Parser s [s]
pseq = let f = \pa pb -> fmap (\t -> (fst t) ++ (snd t)) (pa .>>. pb)
           toList = fmap (:[]) . pfor
       in foldl f (pure []) . fmap (toList)

many :: Parser s a -> Parser s [a]
many (Parser doParse pl) = newParser <%> ((brackets . plabel $ newParser) ++ "*")
    where newParser = Parser (\t -> parse t) (pl)
          parse text = case (doParse text) of (Failure err) -> Result ([], text)
                                              (Result (value, remText)) -> fmap (first (value:)) . parse $ remText

many1 :: Parser s a -> Parser s [a]
many1 parser = (do
    r1 <- parser
    r2 <- many parser
    pure (r1:r2)) <%> ((plabel parser) ++ "+")


opt :: Parser s a -> Parser s (Maybe a)
opt parser = (fmap Just parser) <|> (return Nothing) <%> ((plabel parser) ++ "?")

assertParser :: (Show a) => String -> Parser a (Maybe b) -> Parser a b
assertParser message (Parser func l) = Parser (\text -> 
    case func text of 
        (Result ((Just r), inputState)) -> Result (r, inputState)
        (Result ((Nothing), inputState)) -> Failure (l, message, (toParserPos inputState))
        (Failure (fl, message, pos)) -> Failure (fl, message, pos)
    ) l


-------------------- Generic Higher-Order Parser declerations --------------------


pword :: Parser Char String
pword = many1 . anyOf $ "abcdefghijklmnopqrstuvwxyz"

pint :: Parser Char Integer
pint = fmap (read . combine) ((opt . pchar $ '-') .>>. (many1 . anyOf $ "0123456789")) <%> "Integer" where
    combine t = 
        let firstChar = mGetOrElse (fst t) '0'
        in firstChar:(snd t)

w :: Parser Char String
w = many (pchar ' ' <|> pchar '\t' <|> pchar '\n')

pBrackets :: Parser Char a -> Parser Char a
pBrackets parser = pchar '(' >> w >>. parser .>> (w >> (pchar ')'))


-------------------- Custom Interpreter Stuff using Parsers --------------------

-- symbol table -- 

type Symbol = String
type SymbolList = [String]
type SymbolTable a = [(Symbol, a)]

lookupSymbol :: SymbolTable a -> Symbol -> Maybe a
lookupSymbol [] sym = Nothing
lookupSymbol (x:xs) sym
    | fst x == sym = Just $ snd x
    | otherwise = lookupSymbol xs sym

bindST :: SymbolTable a -> String -> a -> SymbolTable a
bindST st s x = st ++ [(s, x)]

-- AST model -- 

data Expression = CEXP Context | MEXP MathExp deriving Show

data Context = Context {
    symbolStack :: Stack String,
    symbolTable :: SymbolTable Expression,
    expression :: Expression
    } deriving Show

data MathOpType = Add | Sub | Mul deriving Show
data MathExp = MFunc Symbol (Stack Expression) | MVariable Symbol | MConstant Integer | MNode MathOpType Expression Expression deriving Show

-- evaluate AST --

meval :: SymbolTable Expression -> Expression -> Either String Integer
meval st (MEXP (MConstant x)) = Right x
meval st (MEXP (MNode Add x y)) = do 
    a <- (meval st x)
    b <- (meval st y)
    Right $ a + b
meval st (MEXP (MNode Sub x y)) = do 
    a <- (meval st x)
    b <- (meval st y)
    Right $ a - b
meval st (MEXP (MNode Mul x y)) = do 
    a <- (meval st x)
    b <- (meval st y)
    Right $ a * b

meval st (MEXP (MVariable symbol)) = maybeToEither errMessage (lookupSymbol st symbol) >>= meval st where
    errMessage = "Symbol " ++ symbol ++ " not found"

meval st (MEXP (MFunc funcSymbol argStack)) = maybeToEither errMessage (lookupSymbol st funcSymbol) >>= compute where
    errMessage = "Function " ++ funcSymbol ++ " not found"
    compute :: Expression -> Either String Integer
    compute (MEXP mExp) = Left $ "Illegal invocation of math expression as a function\nSymbol: " ++ funcSymbol
    compute (CEXP (Context ss nst exp)) = meval (bindArgs argStack ss) exp where
            bindArgs :: Stack Expression -> Stack String -> SymbolTable Expression
            bindArgs argStack symStack = mGetOrElse (do
                argMExp <- peek argStack
                boundVar <- peek symStack
                Just $ bindST (recursedST) boundVar argMExp) (st ++ nst) where
                    newArgStack = snd . pop $ argStack
                    newSymStack = snd . pop $ symStack
                    recursedST = bindArgs newArgStack newSymStack

meval st (CEXP (Context ss nst exp)) = meval (st ++ nst) exp

-- parse text to AST --

parseMathExp :: Parser Char MathExp
parseMathExp =  let mathOp = fmap doMathOpp $ anyOf "+-*" .>>. (w >>. pBrackets openExp) .>>. (w >>. pBrackets openExp)
                    funcCall = fmap doFuncOpp $ pchar '!' >>. pword .>>. (many (w >>. pBrackets openExp))
                    constant = fmap MConstant pint
                    variable = fmap MVariable pword
                    term = constant <|> funcCall <|> variable <|> mathOp 
                in  (pBrackets term <|> term) <%> "Math Expression" where
                    doMathOpp (('+', ls), rs) = MNode Add ls rs
                    doMathOpp (('-', ls), rs) = MNode Sub ls rs
                    doMathOpp (('*', ls), rs) = MNode Mul ls rs
                    doFuncOpp (symbol, argList) = MFunc symbol (Stack argList)


parseFuncCall :: Parser Char Context
parseFuncCall = fmap f $ parseFunc .>>. (many (w >>. pBrackets openExp)) where

    toFuncExp :: (String, Expression) -> Context
    toFuncExp (var, (CEXP (Context symStack st exp))) = Context (push var symStack) st exp
    toFuncExp (var, (MEXP mExp)) = Context (Stack [var]) [] (MEXP mExp)
    parseFunc = fmap toFuncExp $ pchar '|' >>. (pword .>> pchar '.') .>>. pBrackets openExp

    f :: (Context, [Expression]) -> Context
    f (context, ([])) = context
    f ((Context ss st exp), (x:xs)) = 
        let rest = f (Context poppedStack st exp, xs)
            poppedStack = snd . pop $ ss
            ans = do
                value <- fst . pop $ ss
                return $ Context (symbolStack rest) (bindST (symbolTable rest) value x) exp
        in  mGetOrElse (ans) (Context newStack st exp) 

openExp :: Parser Char Expression
openExp = (fmap CEXP parseFuncCall <|> fmap MEXP parseMathExp) <%> "Expression"

debug :: Parser Char (Either String Integer)
debug = fmap (meval []) (w >>. openExp)
-- debug :: Parser Char Expression
-- debug = openExp


