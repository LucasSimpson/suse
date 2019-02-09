import Data.Bifunctor
import Data.Maybe
import Data.Either
import Control.Monad

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
        solved = fmap (solve (newScope [])) parsed

        execute (Result (exp, s)) = show . solve (newScope []) $ exp
        execute (failure) = show failure

-------------------- generic helper dat / funcs --------------------

maybeToEither :: String -> Maybe a -> Either String a
maybeToEither _ (Just x) = Right x
maybeToEither errMessage (Nothing) = Left errMessage

mOr :: Maybe a -> Maybe a -> Maybe a
mOr (Just x) m2 = Just x
mOr (Nothing) m2 = m2

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

sizeOf :: Stack a -> Int
sizeOf (Stack s) = length s

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
pword = many1 . anyOf $ "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

pint :: Parser Char Integer
pint = fmap (read . combine) ((opt . pchar $ '-') .>>. (many1 . anyOf $ "0123456789")) <%> "Integer" where
    combine t = 
        let firstChar = mGetOrElse (fst t) '0'
        in firstChar:(snd t)

w :: Parser Char String
w = many (pchar ' ' <|> pchar '\t' <|> pchar '\n')

pBrackets :: Parser Char a -> Parser Char a
pBrackets parser = pchar '(' >> w >>. parser .>> (w >> (pchar ')'))

pCurlyBrackets :: Parser Char a -> Parser Char a
pCurlyBrackets parser = pchar '{' >> w >>. parser .>> (w >> (pchar '}'))

-------------------- Custom Interpreter Stuff using Parsers --------------------

-- symbol table -- 

type Symbol = String
type SymbolList = [String]
type SymbolTable a = [(Symbol, a)]

stLookup :: Symbol -> SymbolTable a -> Maybe a
stLookup sym [] = Nothing
stLookup sym (x:xs)
    | fst x == sym = Just $ snd x
    | otherwise = stLookup sym xs

stBind :: SymbolTable a -> String -> a -> SymbolTable a
stBind st s x = st ++ [(s, x)]

data Scope a = Scope {
        symbolTable :: SymbolTable a,
        parent :: Maybe (Scope a)
    }

instance (Show a) => Show (Scope a) where 
    show scope = "Some Scope"
    -- show (Scope st mParent) = mGetOrElse (fmap f mParent) (showSt st) where
    --     f pScope = showSt st ++ "\n" ++ show pScope

    --     showSt [] = ""
    --     showSt (p:xs) = (show . fst $ p) ++ " -> " ++ (show . snd $ p) ++ "\n" ++ showSt xs

newScope :: SymbolTable a -> Scope a
newScope st = Scope st Nothing

setParent :: Scope a -> Scope a -> Scope a
setParent parent child = Scope (symbolTable child) (Just parent) 

lookupSymbol :: Symbol -> Scope a -> Maybe a
lookupSymbol sym (Scope st parent) = mOr (stLookup sym st) (parent >>= lookupSymbol sym) 

bindToScope :: Scope a -> Symbol -> a -> Scope a
bindToScope (Scope st parent) sym value = Scope (stBind st sym value) parent

-- AST model -- 

data Constant = Constant Integer deriving Show
data BoolLiteral = BoolLiteral Bool deriving Show
data FunctionContext = FunctionContext {
        funcName :: Symbol,
        symbolStack :: Stack String,
        scope :: Scope Expression,
        funcExpression :: Expression
    } deriving Show

data BuiltInFunc = BuiltInFunc {
        name :: String,
        func :: Scope Expression -> Executer ComputationResult
    }

data ExpressionLookup = ExpressionLookup String deriving Show
data FunctionApplication = FunctionApplication { 
        previousExpression :: Expression,
        argument :: Expression  
    } deriving Show

data IfStatement = IfStatement Expression Expression Expression deriving Show

data Expression =   ExpFunctionContext FunctionContext |  
                    ExpConst Constant |
                    ExpBoolLiteral BoolLiteral |
                    ExpIfStatement IfStatement |
                    ExpFunctionApplication FunctionApplication |
                    ExpExpressionLookup ExpressionLookup |
                    ExpBuiltInFunc BuiltInFunc

instance Show BuiltInFunc where
    show (BuiltInFunc name f) = "BuiltIn(" ++ show name ++ ")"

instance Show Expression where
    show (ExpFunctionContext fc) = show fc
    show (ExpConst const) = show const
    show (ExpFunctionApplication fa) = show fa
    show (ExpExpressionLookup el) = show el
    show (ExpBuiltInFunc bif) = show bif
    show (ExpIfStatement ie) = show ie
    show (ExpBoolLiteral bl) = show bl

-- runtime monad --

data Executer a = Executer { logLines :: [String], solvedResult :: Either String a }

instance Functor Executer where
    fmap f (Executer logs result) = Executer logs $ fmap f result

instance Applicative Executer where
    pure x = Executer [] $ pure x
    (Executer l1 r1) <*> (Executer l2 r2) = Executer (l2 ++ l1) (r1 <*> r2)

instance Monad Executer where
    return = pure
    executer >>= f = infCheck $ doBind (infCheck executer) f where

            infCheck :: Executer a -> Executer a
            infCheck (Executer logs r) = if (length logs <= 1000) then (
                    Executer logs r
                ) else (Executer logs $ Left "Stopped automatically after too many log lines")

            doBind :: Executer a -> (a -> Executer b) -> Executer b
            doBind (Executer logs (Left msg)) f = Executer logs (Left msg)
            doBind (Executer logs (Right x)) f = 
                let newExecuter = f x
                in  Executer ((logLines newExecuter) ++ logs) (solvedResult newExecuter)


instance (Show a) => Show (Executer a) where
    show (Executer logs r) = showLogs logs ++ "\n\n" ++ (show r) where
        showLogs [] = ""
        showLogs (x:xs) = (showLogs xs) ++ "\n" ++ x

pureFlop :: String -> Executer a
pureFlop msg = Executer [] $ Left msg

appendLog :: Executer a -> String -> Executer a
appendLog (Executer logs r) newLog = Executer (newLog:logs) r
( %% ) = appendLog
( %%. ) = appendLog

prependLog :: Executer a -> String -> Executer a
prependLog (Executer logs r) newLog = Executer (logs ++ [newLog]) r
( .%% ) = prependLog

appendContextLog :: Executer a -> (a -> String) -> Executer a
appendContextLog (Executer logs (Right r)) f = Executer logs (Right r) %%. (f r)
appendContextLog el f = el
( >%% ) = appendContextLog
( >%%. ) = appendContextLog

prependContextLog :: Executer a -> (a -> String) -> Executer a
prependContextLog (Executer logs (Right r)) f = Executer logs (Right r) .%% (f r)
prependContextLog el f = el
( >.%% ) = prependContextLog

-- evaluate AST --

data ComputationResult =    TInt Integer | 
                            FC FunctionContext |
                            TBool Bool

                            deriving Show

class Solvable a where
    solve :: Scope Expression -> a -> Executer ComputationResult
    logShow :: a -> String


instance Solvable Constant where
    solve scope (Constant x) = (pure . TInt $ x) %% ("Found " ++ show x)
    logShow (Constant x) = show x

instance Solvable BoolLiteral where
    solve scope (BoolLiteral x) = (pure . TBool $ x) %% ("Found " ++ show x)
    logShow (BoolLiteral x) = show x

instance Solvable FunctionContext where
    solve pScope (FunctionContext name ss cScope exp) = pure . FC $ toSolve where
        combinedScope = setParent pScope cScope
        -- self = FunctionContext name ss combinedScope exp
        -- recursedScope = bindToScope combinedScope name (ExpFunctionContext self)
        -- toSolve = FunctionContext name ss recursedScope exp
        toSolve = FunctionContext name ss combinedScope exp

    logShow (FunctionContext name ss cScope exp) = "(FuncContext for " ++ show name ++ ")"

runFunctionContext :: FunctionContext -> Executer ComputationResult
runFunctionContext (FunctionContext name ss pScope exp) = solve pScope exp .%% ("Fully applied, runing func with scope \n ******* SCOPE ******* \n" ++ show pScope ++ "\n ******* /SCOPE ******* \n") where
    -- self = FunctionContext name ss pScope exp
    -- newScope = bindToScope pScope name (ExpFunctionContext self)
-- runFunctionContext (FunctionContext name ss pScope exp) = solve pScope exp 


instance Solvable IfStatement where
    solve pScope (IfStatement cond trueExp falseExp) = do
        condResult <- solve pScope cond
        bool <- onlyTBool condResult >%%. (\r -> "Evaluated condition to " ++ show r)
        if bool then (solve pScope trueExp) else (solve pScope falseExp)

    logShow ifStatement = "[IfStatement]"


instance Solvable FunctionApplication where
    solve pScope (FunctionApplication prevExp arg) = (solve pScope prevExp) >>= applyCR arg >>= callFunc where

        callFunc :: ComputationResult -> Executer ComputationResult
        callFunc (FC (FunctionContext name ss cScope exp)) = if (sizeOf ss == 0) then (
                -- runFunctionContext . FunctionContext name ss cScope $ exp
                runFunctionContext . FunctionContext name ss (setParent cScope pScope) $ exp
            ) else (
                pure . FC . FunctionContext name ss cScope $ exp
                -- pure . FC . FunctionContext name ss (setParent cScope pScope) $ exp
            )

        applyCR :: Expression -> ComputationResult -> Executer ComputationResult
        applyCR argExp (FC (FunctionContext name ss cScope funcExp)) = 
            let newStack = snd . pop $ ss
                mExpFuncContext = do
                    sym <- fst . pop $ ss
                    newFc <- pure $ FunctionContext name newStack (bindToScope cScope sym argExp) funcExp
                    return ((pure . FC $ newFc) %%. ("Binding " ++ (logShow argExp) ++ " to " ++ (logShow newFc)))
            in mGetOrElse mExpFuncContext (pureFlop "Runtime Exception: To many arguments.")

        applyCR argExp cr = pureFlop $ "Runtime Exception: Tried to call non-function " ++ show cr
        
    logShow funcApp = "[FunctionApplication]"


crForBuiltIn :: Scope Expression -> BuiltInFunc -> Executer ComputationResult
crForBuiltIn pScope (BuiltInFunc name func) = (pure . FC . FunctionContext name (Stack ["_1", "_2"]) pScope . ExpBuiltInFunc $ (BuiltInFunc name func)) %%. ("Built-in func " ++ show name ++ " found")

instance Solvable ExpressionLookup where
    solve pScope (ExpressionLookup symbol)
        | symbol == "+" = crForBuiltIn pScope builtInAdd
        | symbol == "-" = crForBuiltIn pScope builtInSub
        | symbol == "*" = crForBuiltIn pScope builtInMult
        | symbol == "==" = crForBuiltIn pScope builtInEq
        | symbol == ">" = crForBuiltIn pScope builtInGT
        | symbol == "<" = crForBuiltIn pScope builtInLT
        | symbol == "and" = crForBuiltIn pScope builtInAnd
        | symbol == "or" = crForBuiltIn pScope builtInOr
        | otherwise = mGetOrElse (
                do
                    exp <- lookupSymbol symbol pScope
                    pure (solve pScope exp %% ("Binding " ++ logShow exp ++ " to " ++ symbol))
            ) (
                pureFlop $ "Symbol " ++ symbol ++ " not found"
            ) .%% ("Looking for " ++ symbol ++ " with scope\n ***** LOOKUP SCOPE *****\n" ++ show pScope ++ " ***** /LOOKUP SCOPE *****\n")

    logShow (ExpressionLookup sym) = "ExpLookup(" ++ show sym ++ ")"

instance Solvable BuiltInFunc where
    solve pScope (BuiltInFunc name func) = (func pScope) .%% ("Running built-in func " ++ show name)
    logShow (BuiltInFunc name f) = "BF(" ++ show name ++ ")"

instance Solvable Expression where
    solve pScope (ExpFunctionContext exp) = (pure exp >>= solve pScope) .%% "Solving FunctionContext"
    solve pScope (ExpConst exp) = (pure exp >>= solve pScope) .%% "Solving Constant"
    solve pScope (ExpIfStatement exp) = (pure exp >>= solve pScope) .%% "Solving IfStatement"
    solve pScope (ExpFunctionApplication exp) = (pure exp >>= solve pScope) .%% "Solving FunctionApplication"
    solve pScope (ExpExpressionLookup exp) = (pure exp >>= solve pScope) .%% "Solving ExpressionLookup"
    solve pScope (ExpBuiltInFunc exp) = (pure exp >>= solve pScope) .%% "Solving BuiltInFunc"
    solve pScope (ExpBoolLiteral exp) = (pure exp >>= solve pScope) .%% "Solving BoolLiteral"

    logShow (ExpFunctionContext exp) = logShow exp
    logShow (ExpConst exp) = logShow exp
    logShow (ExpBoolLiteral exp) = logShow exp
    logShow (ExpIfStatement exp) = logShow exp
    logShow (ExpFunctionApplication exp) = logShow exp
    logShow (ExpExpressionLookup exp) = logShow exp
    logShow (ExpBuiltInFunc exp) = logShow exp

-- parse text to AST --

parseConstant :: Parser Char Constant
parseConstant = fmap Constant pint

parseBool :: Parser Char BoolLiteral
parseBool = fmap toBool (pseq "True" <|> pseq "False") where
    toBool str
        | str == "True" = BoolLiteral True
        | otherwise     = BoolLiteral False

parseFuncDecleration :: Parser Char FunctionContext
parseFuncDecleration = fmap toFuncExp $ pchar '|' >>. pword .>>. pBrackets pword .>>. (w >>. pCurlyBrackets openExp) where
    toFuncExp :: ((String, String), Expression) -> FunctionContext
    toFuncExp ((name, var), exp) = self where
        scope = bindToScope (newScope []) name (ExpFunctionContext self)
        self = FunctionContext name (Stack [var]) (scope) exp
    
parseIfStatement :: Parser Char IfStatement
parseIfStatement = fmap toIf $ pseq "if" >>. oe .>>. (pseq "then" >>. oe) .>>. (pseq "else" >>. oe) where
    oe = w >>. pBrackets openExp .>> w
    toIf ((condExp, trueExp), falseExp) = IfStatement condExp trueExp falseExp

parseExpressionLookup :: Parser Char ExpressionLookup
parseExpressionLookup = fmap ExpressionLookup (boolOpp <|> mathOpp <|> variable) where
    variable = pword
    mathOpp = fmap (:"") . anyOf $ "+-*"
    boolOpp = pseq "==" <|> pseq "and" <|> pseq "or" <|> (fmap (:"") . anyOf $ "<>")


singleTerm :: Parser Char Expression
singleTerm = (constant <|> bool <|> ifStatement <|> funcDecleration <|> expLookup <|> pBrackets singleTerm) <%> "Single Term" where
    constant = fmap ExpConst parseConstant
    bool = fmap ExpBoolLiteral parseBool
    ifStatement = fmap ExpIfStatement parseIfStatement
    funcDecleration = fmap ExpFunctionContext parseFuncDecleration
    expLookup = fmap ExpExpressionLookup parseExpressionLookup


applyExpression :: FunctionContext -> Expression -> Parser Char Expression
applyExpression (FunctionContext name ss pScope exp) value = 
    let newStack = snd . pop $ ss
        mExpFuncContext = do
            sym <- fst . pop $ ss
            return . pure . ExpFunctionContext $ FunctionContext name newStack (bindToScope pScope sym value) exp
    in mGetOrElse mExpFuncContext (failed "Too many function applications" "StackPop")

parseFuncApply :: Parser Char Expression
parseFuncApply = pBrackets openExp

openExp :: Parser Char Expression
openExp = ((singleTerm .>>. many (w >>. parseFuncApply)) >>= flatten) <%> "Open Expression" where
    flatten :: (Expression, [Expression]) -> Parser Char Expression
    flatten (base, stack) = foldl (\p e1 -> p >>= reduce e1) (pure base) stack

    reduce :: Expression -> Expression -> Parser Char Expression
    reduce e1 e2 = pure . ExpFunctionApplication . FunctionApplication e2 $ e1

-- built in functions / stdlib --

onlyTInt :: ComputationResult -> Executer Integer
onlyTInt (TInt x) = pure x
onlyTInt other = pureFlop $ "Runtime Exception: Expected Int, got " ++ show other

onlyTBool :: ComputationResult -> Executer Bool
onlyTBool (TBool x) = pure x
onlyTBool other = pureFlop $ "Runtime Exception: Expected Bool, got " ++ show other

onlyFC :: ComputationResult -> Executer FunctionContext
onlyFC (FC x) = pure x
onlyFC other = pureFlop $ "Runtime Exception: Expected FunctionContext, got " ++ show other


twoArgBuiltInFunc :: String -> (ComputationResult -> ComputationResult -> Executer ComputationResult) -> BuiltInFunc
twoArgBuiltInFunc name doOpp = BuiltInFunc name f where
    f :: Scope Expression -> Executer ComputationResult
    f pScope = mGetOrElse (do
        arg1 <- lookupSymbol "_1" pScope
        arg2 <- lookupSymbol "_2" pScope 
        pure (
                do
                    r1 <- solve pScope arg1
                    r2 <- solve pScope arg2
                    doOpp r1 r2 
            )
        ) (pureFlop $ "Arg lookup failure on built-in func " ++ name) 

builtInAdd :: BuiltInFunc 
builtInAdd = twoArgBuiltInFunc "Add" doOpp where
    doOpp :: ComputationResult -> ComputationResult -> Executer ComputationResult
    doOpp cx cy = do
        x <- onlyTInt cx
        y <- onlyTInt cy
        (pure . TInt $ (x + y)) >%%. (\r -> show x ++ " + " ++ show y ++ " = " ++ show r)


builtInSub :: BuiltInFunc 
builtInSub = twoArgBuiltInFunc "Sub" doOpp where
    doOpp :: ComputationResult -> ComputationResult -> Executer ComputationResult
    doOpp cx cy = do
        x <- onlyTInt cx
        y <- onlyTInt cy
        (pure . TInt $ (x - y)) >%%. (\r -> show x ++ " - " ++ show y ++ " = " ++ show r)


builtInMult :: BuiltInFunc 
builtInMult = twoArgBuiltInFunc "Mult" doOpp where
    doOpp :: ComputationResult -> ComputationResult -> Executer ComputationResult
    doOpp cx cy = do
        x <- onlyTInt cx
        y <- onlyTInt cy
        (pure . TInt $ (x * y)) >%%. (\r -> show x ++ " * " ++ show y ++ " = " ++ show r)


builtInEq :: BuiltInFunc
builtInEq = twoArgBuiltInFunc "Eq" doOpp where
    doOpp :: ComputationResult -> ComputationResult -> Executer ComputationResult
    doOpp (TBool x) (TBool y) = (pure . TBool $ (x == y)) >%%. (\r -> show x ++ "==" ++ show y ++ " -> " ++ show r)
    doOpp (TInt x) (TInt y) = (pure . TBool $ (x == y)) >%%. (\r -> show x ++ "==" ++ show y ++ " -> " ++ show r)
    doOpp x y = pureFlop $ "Cannot equate two args of different types: " ++ show x ++ " vs " ++ show y


builtInGT :: BuiltInFunc
builtInGT = twoArgBuiltInFunc "GT" doOpp where
    doOpp cx cy = do
        x <- onlyTInt cx
        y <- onlyTInt cy
        (pure . TBool $ (x > y)) >%%. (\r -> show x ++ " > " ++ show y ++ " -> " ++ show r)

builtInLT :: BuiltInFunc
builtInLT = twoArgBuiltInFunc "LT" doOpp where
    doOpp cx cy = do
        x <- onlyTInt cx
        y <- onlyTInt cy
        (pure . TBool $ (x < y)) >%%. (\r -> show x ++ " < " ++ show y ++ " -> " ++ show r)


builtInAnd :: BuiltInFunc
builtInAnd = twoArgBuiltInFunc "And" doOpp where
    doOpp cx cy = do
        x <- onlyTBool cx
        y <- onlyTBool cy
        (pure . TBool $ (x && y)) >%%. (\r -> show x ++ " && " ++ show y ++ " -> " ++ show r)

builtInOr :: BuiltInFunc
builtInOr = twoArgBuiltInFunc "Or" doOpp where
    doOpp cx cy = do
        x <- onlyTBool cx
        y <- onlyTBool cy
        (pure . TBool $ (x || y)) >%%. (\r -> show x ++ " || " ++ show y ++ " -> " ++ show r)
