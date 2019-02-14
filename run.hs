import Data.Bifunctor
import Data.Maybe
import Data.Either
import Control.Monad

import System.IO  

import Parsing
import Utils
  
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
data ListLiteral = ListLiteral [Expression] deriving Show

data ListIndex = ListIndex Expression Expression deriving Show

data FunctionContext = FunctionContext {
        funcName :: Symbol,
        symbolStack :: Stack String,
        scope :: Scope ComputationResult,
        funcExpression :: Expression
    } deriving Show

data BuiltInFunc = BuiltInFunc {
        name :: String,
        func :: Scope ComputationResult -> Executer ComputationResult
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
                    ExpListLiteral ListLiteral |
                    ExpListIndex ListIndex |
                    ExpIfStatement IfStatement |
                    ExpFunctionApplication FunctionApplication |
                    ExpExpressionLookup ExpressionLookup |
                    ExpBuiltInFunc BuiltInFunc

instance Show BuiltInFunc where
    show (BuiltInFunc name f) = "BuiltIn(" ++ show name ++ ")"

instance Show Expression where
    show (ExpFunctionContext fc) = show fc
    show (ExpConst const) = show const
    show (ExpBoolLiteral bl) = show bl
    show (ExpListLiteral ll) = show ll
    show (ExpListIndex li) = show li
    show (ExpFunctionApplication fa) = show fa
    show (ExpExpressionLookup el) = show el
    show (ExpBuiltInFunc bif) = show bif
    show (ExpIfStatement ie) = show ie

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

data ComputationResult  =   TInt Integer | 
                            FC FunctionContext |
                            TBool Bool |
                            TList [ComputationResult]

                            deriving Show

class Solvable a where
    solve :: Scope ComputationResult -> a -> Executer ComputationResult
    logShow :: a -> String


instance Solvable Constant where
    solve scope (Constant x) = (pure . TInt $ x) %% ("Found " ++ show x)
    logShow (Constant x) = show x

instance Solvable BoolLiteral where
    solve scope (BoolLiteral x) = (pure . TBool $ x) %% ("Found " ++ show x)
    logShow (BoolLiteral x) = show x

instance Solvable ListLiteral where
    solve scope (ListLiteral listExp) = fmap TList . combineExecutors . fmap (solve scope) $ listExp where

        merge :: Executer [ComputationResult] -> Executer ComputationResult -> Executer [ComputationResult]
        merge ex1 ex2 = do
            tList <- ex1
            cr2 <- ex2
            pure $ tList ++ [cr2]

        combineExecutors :: [Executer ComputationResult] -> Executer [ComputationResult]
        combineExecutors executers = foldl merge (pure []) executers

    logShow (ListLiteral listExp) = show listExp 

instance Solvable ListIndex where
    solve scope (ListIndex exp indexExp) = do
        listExp <- solve scope exp >>= onlyTList
        i <- solve scope indexExp >>= onlyTInt
        mGetOrElse (fmap pure $ index i listExp) (pureFlop "ListIndex Failure")

    logShow li = show li

instance Solvable FunctionContext where
    solve pScope (FunctionContext name ss cScope exp) = if (
            sizeOf ss == 0
        ) then (
            solve cScope exp .%% ("Fully bound, running " ++ logShow (FunctionContext name ss cScope exp))
        ) else (
            pure . FC $ FunctionContext name ss boundScope exp
        ) where
            boundScope = setParent pScope cScope

    logShow (FunctionContext name ss cScope exp) = "(FuncContext for " ++ show name ++ ")"


instance Solvable IfStatement where
    solve pScope (IfStatement cond trueExp falseExp) = do
        condResult <- solve pScope cond
        bool <- onlyTBool condResult >%%. (\r -> "Evaluated condition to " ++ show r)
        if bool then (solve pScope trueExp) else (solve pScope falseExp)

    logShow ifStatement = "[IfStatement]"


instance Solvable FunctionApplication where
    solve pScope (FunctionApplication prevExp arg) = (solve pScope prevExp) >>= applyCR arg where
        applyCR :: Expression -> ComputationResult -> Executer ComputationResult
        applyCR argExp (FC (FunctionContext name ss cScope funcExp)) = 
            let newStack = snd . pop $ ss
                mExpFuncContext = do
                    sym <- fst . pop $ ss
                    x <- pure (do
                            argCr <- (solve pScope argExp) >%%. (\r -> "Solved arg, got " ++ show r)
                            solve pScope (FunctionContext name newStack (bindToScope cScope sym argCr) funcExp)
                        )
                    return x
            in mGetOrElse mExpFuncContext (pureFlop "Runtime Exception: To many arguments.")

        applyCR argExp cr = pureFlop $ "Runtime Exception: Tried to call non-function " ++ show cr
        
    logShow funcApp = "[FunctionApplication]"


crForBuiltIn :: Scope ComputationResult -> BuiltInFunc -> Executer ComputationResult
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
                    cr <- lookupSymbol symbol pScope
                    pure (pure cr %% ("Binding " ++ show cr ++ " to " ++ symbol)) 
            ) (
                pureFlop $ "Symbol " ++ symbol ++ " not found"
            ) .%% ("Looking for " ++ symbol ++ " with scope " ++ show pScope)

    logShow (ExpressionLookup sym) = "ExpLookup(" ++ show sym ++ ")"

instance Solvable BuiltInFunc where
    solve pScope (BuiltInFunc name func) = (func pScope) .%% ("Running built-in func " ++ show name)
    logShow (BuiltInFunc name f) = "BF(" ++ show name ++ ")"

instance Solvable Expression where
    solve pScope (ExpFunctionContext exp) = (pure exp >>= solve pScope) .%% ("Solving FunctionContext with scope " ++ show pScope)
    solve pScope (ExpConst exp) = (pure exp >>= solve pScope) .%% "Solving Constant"
    solve pScope (ExpBoolLiteral exp) = (pure exp >>= solve pScope) .%% "Solving BoolLiteral"
    solve pScope (ExpListLiteral exp) = (pure exp >>= solve pScope) .%% "Solving ListLiteral"
    solve pScope (ExpListIndex exp) = (pure exp >>= solve pScope) .%% "Solving ListIndex"
    solve pScope (ExpIfStatement exp) = (pure exp >>= solve pScope) .%% "Solving IfStatement"
    solve pScope (ExpFunctionApplication exp) = (pure exp >>= solve pScope) .%% ("Solving FunctionApplication with scope " ++ show pScope)
    solve pScope (ExpExpressionLookup exp) = (pure exp >>= solve pScope) .%% "Solving ExpressionLookup"
    solve pScope (ExpBuiltInFunc exp) = (pure exp >>= solve pScope) .%% ("Solving BuiltInFunc with scope " ++ show pScope)

    logShow (ExpFunctionContext exp) = logShow exp
    logShow (ExpConst exp) = logShow exp
    logShow (ExpBoolLiteral exp) = logShow exp
    logShow (ExpListLiteral exp) = logShow exp
    logShow (ExpListIndex exp) = logShow exp
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

parseList :: Parser Char ListLiteral
parseList = fmap toList $ pchar '[' >>. opt (p1 .>>. p2) .>> pchar ']' where
    p1 = opt (many (w >>. openExp .>> w .>> pchar ','))
    p2 = (w >>. openExp .>> w)
    toList (Just (Just listExp, exp)) = ListLiteral $ listExp ++ [exp]
    toList (Just (Nothing, exp)) = ListLiteral [exp]
    toList (Nothing) = ListLiteral []

parseFuncDecleration :: Parser Char FunctionContext
parseFuncDecleration = fmap toFuncExp $ pchar '|' >>. pword .>>. pBrackets pword .>>. (w >>. pCurlyBrackets openExp) where
    toFuncExp :: ((String, String), Expression) -> FunctionContext
    toFuncExp ((name, var), exp) = self where
        scope = bindToScope (newScope []) name (FC self)
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
singleTerm = (constant <|> bool <|> list <|> ifStatement <|> funcDecleration <|> expLookup <|> pBrackets singleTerm) <%> "Single Term" where
    constant = fmap ExpConst parseConstant
    bool = fmap ExpBoolLiteral parseBool
    list = fmap ExpListLiteral parseList
    ifStatement = fmap ExpIfStatement parseIfStatement
    funcDecleration = fmap ExpFunctionContext parseFuncDecleration
    expLookup = fmap ExpExpressionLookup parseExpressionLookup


parseFuncApply :: Parser Char Expression
parseFuncApply = pBrackets openExp

-- parseListIndex :: Parser Char ListIndex
-- parseListIndex = fmap toListIndex $ openExp .>> w .>> pchar '@' .>> w .>>. pBrackets openExp where
--     toListIndex (exp, indexExp) = ListIndex exp indexExp

parseListIndex :: Parser Char Expression
parseListIndex = pseq "!!" >> w >>. pBrackets openExp 

-- openExp' :: Parser Char Expression
-- openExp' = ((singleTerm .>>. many (w >>. parseFuncApply)) >>= flatten) <%> "Open Expression" where
--     flatten :: (Expression, [Expression]) -> Parser Char Expression
--     flatten (base, stack) = foldl (\p e1 -> p >>= reduce e1) (pure base) stack

--     reduce :: Expression -> Expression -> Parser Char Expression
--     reduce e1 e2 = pure . ExpFunctionApplication . FunctionApplication e2 $ e1

    -- no = singleTerm >>= r2

    -- r2 :: Expression -> Parser Char Expression -> Parser Char Expression
    -- r2 exp pexp = (fmap fa parseFuncApply) <|> (fmap fb parseListIndex)

    -- fa (exp1, exp2) = pure . ExpFunctionApplication . FunctionApplication exp2 $ exp1
    -- fb (exp1, exp2) = pure . ExpListIndex . ListIndex exp2 $ exp1

-- pConsume :: (a -> Parser s a) -> Parser s a -> Parser s a

makeFuncApplication :: Expression -> Parser Char Expression
makeFuncApplication e1 = (w >>. parseFuncApply) >>= \e2 -> 
    pure . ExpFunctionApplication . FunctionApplication e1 $ e2


makeListIndex :: Expression -> Parser Char Expression
makeListIndex e1 = (w >>. parseListIndex) >>= \e2 -> 
    pure . ExpListIndex . ListIndex e1 $ e2


openExp :: Parser Char Expression
openExp = pConsume cParser singleTerm where
    cParser exp = (makeFuncApplication exp) <|> (makeListIndex exp) 


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

onlyTList :: ComputationResult -> Executer [ComputationResult]
onlyTList (TList x) = pure x
onlyTList other = pureFlop $ "Runtime Exception: Expected List, got " ++ show other

twoArgBuiltInFunc :: String -> (ComputationResult -> ComputationResult -> Executer ComputationResult) -> BuiltInFunc
twoArgBuiltInFunc name doOpp = BuiltInFunc name f where
    f :: Scope ComputationResult -> Executer ComputationResult
    f pScope = mGetOrElse (do
            arg1 <- lookupSymbol "_1" pScope
            arg2 <- lookupSymbol "_2" pScope 
            pure $ doOpp arg1 arg2
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
