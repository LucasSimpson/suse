import Data.Bifunctor
import Data.Maybe
import Data.Either

import System.IO  
  
-------------------- MAIN EXECUTION --------------------
-- read from file, parse, execute, show --

main = do  
    handle <- openFile "input.txt" ReadMode  
    contents <- hGetContents handle
    putStr . (flip (++)) "\n" . pullInfo . rp parsed . splitNL $ contents
    putStr . (flip (++)) "\n" . execute . rp parsed . splitNL $ contents
    hClose handle where
        pullInfo (Result (exp, s)) = show exp 
        pullInfo (failure) = show failure 
        parsed = w >>. openExp
        solved = fmap (solve []) parsed

        execute (Result (exp, s)) = show . solve [] $ exp
        execute (failure) = show failure

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

failed :: (Show s) => String -> String -> Parser s a 
failed message label = Parser (\inputState ->   let (mToken, remInput) = nextToken inputState
                                                in Failure (label, message, toParserPos remInput)
    ) label

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


data MathOpType = Add | Sub | Mul deriving Show
data MathOp = MNode MathOpType Expression Expression deriving Show

data Constant = Constant Integer deriving Show
data BoundVar = BoundVar Symbol deriving Show
data FunctionCall = FunctionCall Symbol (Stack Expression) deriving Show
data FunctionContext = FunctionContext {
        symbolStack :: Stack String,
        symbolTable :: SymbolTable Expression,
        expression :: Expression
    } deriving Show

data IfStatement = IfStatement Expression Expression Expression deriving Show

data Expression =   ExpFunctionContext FunctionContext | 
                    ExpMathOp MathOp | 
                    ExpConst Constant |
                    ExpBoundVar BoundVar |
                    ExpFunctionCall FunctionCall |
                    ExpIfStatement IfStatement

                    deriving Show

-- ExecutionLog monad --

data ExecutionLog a = ExecutionLog { logs :: [String], result :: a }

instance Functor ExecutionLog where
    fmap f (ExecutionLog ls r) = ExecutionLog ls (f r)

instance Applicative ExecutionLog where 
    pure x = ExecutionLog [] x
    el1 <*> el2 = ExecutionLog ((logs el2) ++ (logs el1)) ((result el1) (result el2))

instance Monad ExecutionLog where
    return = pure
    ExecutionLog ls r >>= f =   
        let nel = f r
        in  ExecutionLog ((logs nel) ++ ls) (result nel)

instance (Show a) => Show (ExecutionLog a) where
    show (ExecutionLog ls r) = logLines ls ++ "\n\n" ++ (show r) where
        logLines [] = ""
        logLines (x:xs) = (logLines xs) ++ "\n" ++ x

appendLog :: ExecutionLog a -> String -> ExecutionLog a
appendLog (ExecutionLog ls r) newLog = ExecutionLog (newLog:ls) r
( %% ) = appendLog
( %%. ) = appendLog

prependLog :: ExecutionLog a -> String -> ExecutionLog a
prependLog (ExecutionLog ls r) newLog = ExecutionLog (ls ++ [newLog]) r
( .%% ) = prependLog

appendContextLog :: ExecutionLog (Either String a) -> (a -> String) -> ExecutionLog (Either String a)
appendContextLog (ExecutionLog ls (Right r)) f = ExecutionLog ls (Right r) %%. (f r)
appendContextLog el f = el
( >%% ) = appendContextLog
( >%%. ) = appendContextLog

prependContextLog :: ExecutionLog (Either String a) -> (a -> String) -> ExecutionLog (Either String a)
prependContextLog (ExecutionLog ls (Right r)) f = ExecutionLog ls (Right r) .%% (f r)
prependContextLog el f = el
( >.%% ) = prependContextLog

-- evaluate AST --

class Solvable a where
    solve :: SymbolTable Expression -> a -> ExecutionLog (Either String Integer)

invert :: Either String (ExecutionLog (Either String a)) -> ExecutionLog (Either String a)
invert (Left msg) = pure . Left $ msg
invert (Right (ExecutionLog ls er)) = ExecutionLog ls er


instance Solvable Constant where
    solve st (Constant x) = (pure . Right $ x) %% ("Found " ++ show x)


instance Solvable BoundVar where
    solve st (BoundVar symbol) = invert result .%% ("Looking for value " ++ symbol) where
        errMessage = "Symbol " ++ symbol ++ " not found"
        result = do
            exp <- maybeToEither errMessage (lookupSymbol st symbol)
            pure (solve st exp %% ("Bound result to " ++ symbol))


instance Solvable FunctionCall where
    solve st (FunctionCall funcSymbol argStack) = invert result .%% ("Calling func bound to " ++ funcSymbol) where
        ncompute :: Expression -> ExecutionLog (Either String Integer)
        ncompute (ExpFunctionContext (FunctionContext ss nst exp)) = solve (bindArgs argStack ss) exp where
                bindArgs :: Stack Expression -> Stack String -> SymbolTable Expression
                bindArgs argStack symStack = mGetOrElse (do
                    argMExp <- peek argStack
                    boundVar <- peek symStack
                    Just $ bindST (recursedST) boundVar argMExp) (st ++ nst) where
                        newArgStack = snd . pop $ argStack
                        newSymStack = snd . pop $ symStack
                        recursedST = bindArgs newArgStack newSymStack
        ncompute exp = pure (Left $ "Illegal invocation of non-function expression as a function\nSymbol: " ++ funcSymbol)
    
        errMessage = "Function " ++ funcSymbol ++ " not found"

        result = do
            exp <- maybeToEither errMessage (lookupSymbol st funcSymbol)
            pure (ncompute exp)


instance Solvable MathOp where
    solve st (MNode Add x y) = do 
        e1 <- (solve st x)
        e2 <- (solve st y)
        return (pure (+) <*> e1 <*> e2) >%%. (\r -> "Adding, got " ++ show r)

    solve st (MNode Sub x y) = do 
        e1 <- (solve st x)
        e2 <- (solve st y)
        return (pure (-) <*> e1 <*> e2) >%%. (\r -> "Subtracting, got " ++ show r)

    solve st (MNode Mul x y) = do 
        e1 <- (solve st x)
        e2 <- (solve st y)
        return (pure (*) <*> e1 <*> e2) >%%. (\r -> "Mutiplying, got " ++ show r)


instance Solvable FunctionContext where
    solve st (FunctionContext ss nst exp) = solve (st ++ nst) exp


instance Solvable IfStatement where
    solve st (IfStatement cond trueExp falseExp) = do
        elCond <- solve st cond >%%. (\r -> "Evaluated condition to " ++ show r ++ " -> " ++ show (sb r))
        if (seb elCond) then (solve st trueExp) else (solve st falseExp) where
            sb :: Integer -> Bool
            sb n = n > 0

            seb :: Either String Integer -> Bool
            seb (Left msg) = False
            seb (Right n) = sb n


instance Solvable Expression where
    solve st (ExpMathOp exp) = solve st exp
    solve st (ExpFunctionContext exp) = solve st exp
    solve st (ExpConst exp) = solve st exp
    solve st (ExpBoundVar exp) = solve st exp
    solve st (ExpFunctionCall exp) = solve st exp
    solve st (ExpIfStatement exp) = solve st exp


-- parse text to AST --

parseMathExp :: Parser Char Expression
parseMathExp =  let mathOp = fmap doMathOpp $ anyOf "+-*" .>>. (w >>. pBrackets openExp) .>>. (w >>. pBrackets openExp)
                    funcCall = fmap doFuncOpp $ pchar '!' >>. pword .>>. (many (w >>. pBrackets openExp))
                    constant = fmap (ExpConst . Constant) pint
                    variable = fmap (ExpBoundVar . BoundVar) pword
                    term = constant <|> funcCall <|> variable <|> mathOp 
                in  (pBrackets term <|> term) <%> "Math Expression" where
                    doMathOpp (('+', ls), rs) = ExpMathOp (MNode Add ls rs)
                    doMathOpp (('-', ls), rs) = ExpMathOp (MNode Sub ls rs)
                    doMathOpp (('*', ls), rs) = ExpMathOp (MNode Mul ls rs)
                    doFuncOpp (symbol, argList) = ExpFunctionCall . FunctionCall symbol . Stack $ argList

parseConstant :: Parser Char Constant
parseConstant = fmap Constant pint

parseFuncDecleration :: Parser Char FunctionContext
parseFuncDecleration = fmap toFuncExp $ pchar '|' >>. (pword .>> pchar '.') .>>. pBrackets openExp where
    toFuncExp :: (String, Expression) -> FunctionContext
    toFuncExp (var, (ExpFunctionContext (FunctionContext symStack st exp))) = FunctionContext (push var symStack) st exp
    toFuncExp (var, exp) = FunctionContext (Stack [var]) [] (exp)
    

parseIfStatement :: Parser Char IfStatement
parseIfStatement = fmap toIf $ pseq "if" >>. oe .>>. (pseq "then" >>. oe) .>>. (pseq "else" >>. oe) where
    oe = w >>. pBrackets openExp .>> w
    toIf ((condExp, trueExp), falseExp) = IfStatement condExp trueExp falseExp

parseFuncApply :: Parser Char Expression
parseFuncApply = pBrackets openExp

singleTerm :: Parser Char Expression
singleTerm = (constant <|> ifStatement <|> funcDecleration <|> mathOp) <%> "Single Term" where
    constant = fmap ExpConst parseConstant
    ifStatement = fmap ExpIfStatement parseIfStatement
    funcDecleration = fmap ExpFunctionContext parseFuncDecleration
    mathOp = parseMathExp

applyExpression :: FunctionContext -> Expression -> Parser Char Expression
applyExpression (FunctionContext ss st exp) value = 
    let newStack = snd . pop $ ss
        mExpFuncContext = do
            sym <- fst . pop $ ss
            return . pure . ExpFunctionContext $ FunctionContext newStack (bindST st sym value) exp
    in mGetOrElse mExpFuncContext (failed "Too many function applications" "StackPop")

openExp :: Parser Char Expression
openExp = ((singleTerm .>>. many (w >>. parseFuncApply)) >>= flatten) <%> "Open Expression" where
    flatten :: (Expression, [Expression]) -> Parser Char Expression
    flatten (base, stack) = foldl (\p e1 -> p >>= reduce e1) (pure base) stack

    reduce :: Expression -> Expression -> Parser Char Expression
    reduce exp (ExpFunctionContext funcContext) = applyExpression funcContext exp
    reduce e1 e2 = failed "Function application on non-function" "StackPop"




