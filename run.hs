import Data.Bifunctor
import Data.Maybe
import Data.Either
import Control.Monad
import Control.Monad.Trans

import System.IO  

import Parsing
import Utils
  
-------------------- MAIN EXECUTION --------------------
-- read from file, parse, execute, show --

main = do  
    handle <- openFile "input.txt" ReadMode  
    contents <- hGetContents handle
    putStr . (flip (++)) "\n" . pullInfo . rp parsed . splitNL $ contents
    -- display . rp parsed . splitNL $ contents
    execute . rp parsed . splitNL $ contents
    hClose handle where
        pullInfo (Result (exp, s)) = show exp 
        pullInfo (failure) = show failure 
        parsed = w >>. openExp
        solved = fmap (solve (newScope [])) parsed

        execute (Result (exp, s)) = display . runExecuterT . solve (newScope []) $ exp
        execute (failure) = do
            putStr . show $ failure

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

pletter :: Parser Char Char
pletter = anyOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

pnumber :: Parser Char Char
pnumber = anyOf "0123456789"

whitespace :: Parser Char Char 
whitespace = pchar ' ' <|> pchar '\t' <|> pchar '\n'

punctuation :: Parser Char Char
punctuation = anyOf ".,'\"!?-~"

pword :: Parser Char String
pword = many1 pletter

pint :: Parser Char Integer
pint = fmap (read . combine) ((opt . pchar $ '-') .>>. (many1 pnumber)) <%> "Integer" where
    combine t =
        let firstChar = mGetOrElse (fst t) '0'
        in firstChar:(snd t)

w :: Parser Char [String]
w = ws >> many (comment >> ws) where
    comment = pseq "##" >> many (whitespace <|> pnumber <|> pletter <|> punctuation <|> cmsc) >> pseq "##"
    cmsc = anyOf "!@$%^&*()[]{}|+-=/:;><"
    ws = many whitespace

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

instance Show (Scope a) where 
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
data CharLiteral = CharLiteral Char deriving Show
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
        func :: Scope ComputationResult -> ExecuterT IO ComputationResult
    }

data ExpressionLookup = ExpressionLookup String deriving Show
data FunctionApplication = FunctionApplication { 
        previousExpression :: Expression,
        argument :: Expression  
    } deriving Show

data IfStatement = IfStatement Expression Expression Expression deriving Show
data LetStatement = LetStatement [(String, Expression)] Expression deriving Show

data Expression =   ExpFunctionContext FunctionContext |  
                    ExpConst Constant |
                    ExpBoolLiteral BoolLiteral |
                    ExpCharLiteral CharLiteral |
                    ExpListLiteral ListLiteral |
                    ExpListIndex ListIndex |
                    ExpIfStatement IfStatement |
                    ExpLetStatement LetStatement |
                    ExpFunctionApplication FunctionApplication |
                    ExpExpressionLookup ExpressionLookup |
                    ExpBuiltInFunc BuiltInFunc

instance Show BuiltInFunc where
    show (BuiltInFunc name f) = "BuiltIn(" ++ show name ++ ")"

instance Show Expression where
    show (ExpFunctionContext fc) = show fc
    show (ExpConst const) = show const
    show (ExpBoolLiteral bl) = show bl
    show (ExpCharLiteral cl) = show cl
    show (ExpListLiteral ll) = show ll
    show (ExpListIndex li) = show li
    show (ExpFunctionApplication fa) = show fa
    show (ExpExpressionLookup el) = show el
    show (ExpBuiltInFunc bif) = show bif
    show (ExpIfStatement ie) = show ie
    show (ExpLetStatement ls) = show ls

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

executerFlop :: String -> Executer a
executerFlop msg = Executer [] $ Left msg


-- *********** ExecuterT ***************

newtype ExecuterT m a = ExecuterT { runExecuterT :: m (Executer a) }

instance (Functor m) => Functor (ExecuterT m) where
    fmap f (ExecuterT action) = ExecuterT $ fmap (fmap f) action

instance (Monad m) => Applicative (ExecuterT m) where
    pure = ExecuterT . pure . pure
    (ExecuterT a1) <*> (ExecuterT a2) = ExecuterT $ do
        e1 <- a1
        e2 <- a2
        return $ e1 <*> e2

instance (Monad m) => Monad (ExecuterT m) where
    return = pure
    ExecuterT action >>= f = ExecuterT $ do
        r <- action
        case r of 
            (Executer ll (Left msg)) -> return . executerFlop $ msg
            (Executer ll (Right x))  -> (runExecuterT $ f x) >>= addLogs where
                addLogs (Executer logs x) = pure $ Executer (ll ++ logs) x

instance MonadTrans ExecuterT where
    lift x = ExecuterT $ fmap pure x

display :: (Show a) => IO (Executer a) -> IO ()
display iox = do
    exec <- iox
    putStr . show $ exec
    putChar '\n'


pureFlop :: String -> ExecuterT IO a
pureFlop msg = ExecuterT . return . executerFlop $ msg

liftET2 :: (Monad m) => (Executer a -> f -> Executer a) -> (ExecuterT m a -> f -> ExecuterT m a)
liftET2 efunc = eTfunc where
    eTfunc (ExecuterT action) f = ExecuterT $ do
        r <- action
        pure . efunc r $ f

appendLog :: Executer a -> String -> Executer a
appendLog (Executer logs r) newLog = Executer (newLog:logs) r

prependLog :: Executer a -> String -> Executer a
prependLog (Executer logs r) newLog = Executer (logs ++ [newLog]) r

appendContextLog :: Executer a -> (a -> String) -> Executer a
appendContextLog (Executer logs (Right r)) f = appendLog (Executer logs (Right r)) (f r)
appendContextLog el f = el

prependContextLog :: Executer a -> (a -> String) -> Executer a
prependContextLog (Executer logs (Right r)) f = prependLog (Executer logs (Right r)) (f r)
prependContextLog el f = el


appendLogT :: ExecuterT IO a -> String -> ExecuterT IO a
appendLogT = liftET2 appendLog
( %% ) = appendLogT
( %%. ) = appendLogT

prependLogT :: ExecuterT IO a -> String -> ExecuterT IO a
prependLogT = liftET2 prependLog
( .%% ) = prependLogT

appendContextLogT :: ExecuterT IO a -> (a -> String) -> ExecuterT IO a
appendContextLogT = liftET2 appendContextLog
( >%% ) = appendContextLogT
( >%%. ) = appendContextLogT


prependContextLogT :: ExecuterT IO a -> (a -> String) -> ExecuterT IO a
prependContextLogT = liftET2 prependContextLog
( >.%% ) = prependContextLogT


-- evaluate AST --

data ComputationResult  =   TInt Integer | 
                            TBool Bool |
                            TChar Char |
                            TList [ComputationResult] |
                            FC FunctionContext

instance Show ComputationResult where
    show (TInt x) = show x
    show (TBool x) = show x
    show (TChar x) = show x
    show (FC x) = show x
    show (TList l) = if allTChar l then showString l else cShow l where
        allTChar l = foldl (&&) True (fmap isChar l)

        isChar (TChar c) = True
        isChar _ = False

        showString xs = "\"" ++ showString' xs ++ "\""
        showString' ([]) = ""
        showString' (x:xs) = case x of  
            (TChar c) -> c:(showString' xs)
            _ -> (show x) ++ showString' xs

        cShow xs = "[" ++ cShow' xs ++ "]"
        cShow' ([]) = ""
        cShow' (x:[]) = show x
        cShow' (x:xs) = show x ++ ", " ++ cShow' xs

class Solvable a where
    solve :: Scope ComputationResult -> a -> ExecuterT IO ComputationResult
    logShow :: a -> String

instance Solvable ComputationResult where
    solve scope cr = pure cr
    logShow (TInt x) = "TInt " ++ show x
    logShow (TBool x) = "TBool " ++ show x
    logShow (TChar x) = "TChar " ++ show x
    logShow (TList x) = "TList " ++ (show . (fmap logShow) $ x)
    logShow (FC x) = "FC " ++ show x

instance Solvable Constant where
    solve scope (Constant x) = (pure . TInt $ x) %% ("Found " ++ show x)
    logShow (Constant x) = show x

instance Solvable BoolLiteral where
    solve scope (BoolLiteral x) = (pure . TBool $ x) %% ("Found " ++ show x)
    logShow (BoolLiteral x) = show x

instance Solvable CharLiteral where
    solve scope (CharLiteral x) = (pure . TChar $ x) %% ("Found " ++ show x)
    logShow (CharLiteral x) = show x

instance Solvable ListLiteral where
    solve scope (ListLiteral listExp) = fmap TList . combineExecutors . fmap (solve scope) $ listExp where

        merge :: (Monad m) => m [ComputationResult] -> m ComputationResult -> m [ComputationResult]
        merge ex1 ex2 = do
            tList <- ex1
            cr2 <- ex2
            pure $ tList ++ [cr2]

        combineExecutors :: (Monad m) => [m ComputationResult] -> m [ComputationResult]
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
        applyCR :: Expression -> ComputationResult -> ExecuterT IO ComputationResult
        applyCR argExp (FC (FunctionContext name ss cScope funcExp)) = 
            let newStack = snd . pop $ ss
                mExpFuncContext = do
                    sym <- fst . pop $ ss
                    x <- pure (do
                            argCr <- (solve pScope argExp) >%%. (\r -> "Solved arg, got " ++ logShow r)
                            solve pScope (FunctionContext name newStack (bindToScope cScope sym argCr) funcExp)
                        )
                    return x
            in mGetOrElse mExpFuncContext (pureFlop "Runtime Exception: To many arguments.")

        applyCR argExp cr = pureFlop $ "Runtime Exception: Tried to call non-function " ++ logShow cr
        
    logShow funcApp = "[FunctionApplication]"

instance Solvable LetStatement where
    solve pScope (LetStatement boundVars exp) = augmentedScope >>= \s -> solve s exp where
        augmentedScope = foldl f (pure pScope) boundVars

        f :: ExecuterT IO (Scope ComputationResult) -> (String, Expression) -> ExecuterT IO (Scope ComputationResult)
        f execTScope (var, varExp) = do
            scope <- execTScope
            varResult <- solve scope varExp
            pure $ bindToScope scope var varResult

    logShow (LetStatement boundVars exp) = "LetStatement(" ++ show (fmap fst boundVars) ++ ")"


crForBuiltIn :: [String] -> Scope ComputationResult -> BuiltInFunc -> ExecuterT IO ComputationResult
crForBuiltIn argList pScope (BuiltInFunc name func) = (pure . FC . FunctionContext name (Stack argList) pScope . ExpBuiltInFunc $ (BuiltInFunc name func)) .%% ("Built-in func " ++ show name ++ " found")

instance Solvable ExpressionLookup where
    solve pScope (ExpressionLookup symbol)
        | symbol == "+" = crForBuiltIn ["_1", "_2"] pScope builtInAdd
        | symbol == "-" = crForBuiltIn ["_1", "_2"] pScope builtInSub
        | symbol == "*" = crForBuiltIn ["_1", "_2"] pScope builtInMult
        | symbol == "==" = crForBuiltIn ["_1", "_2"] pScope builtInEq
        | symbol == ">" = crForBuiltIn ["_1", "_2"] pScope builtInGT
        | symbol == "<" = crForBuiltIn ["_1", "_2"] pScope builtInLT
        | symbol == "and" = crForBuiltIn ["_1", "_2"] pScope builtInAnd
        | symbol == "or" = crForBuiltIn ["_1", "_2"] pScope builtInOr
        | symbol == "readFile" = crForBuiltIn ["_1"] pScope builtInReadFile
        | symbol == "print" = crForBuiltIn ["_1"] pScope builtInPrint
        | symbol == "len" = crForBuiltIn ["_1"] pScope builtInLength
        | symbol == "slice" = crForBuiltIn ["_1", "_2", "_3"] pScope builtInSlice
        | symbol == "concat" = crForBuiltIn ["_1", "_2"] pScope builtInConcat
        | otherwise = mGetOrElse (do
                cr <- lookupSymbol symbol pScope
                pure (pure cr %% ("Binding " ++ logShow cr ++ " to " ++ symbol)) 
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
    solve pScope (ExpCharLiteral exp) = (pure exp >>= solve pScope) .%% "Solving CharLiteral"
    solve pScope (ExpListLiteral exp) = (pure exp >>= solve pScope) .%% "Solving ListLiteral"
    solve pScope (ExpListIndex exp) = (pure exp >>= solve pScope) .%% "Solving ListIndex"
    solve pScope (ExpIfStatement exp) = (pure exp >>= solve pScope) .%% "Solving IfStatement"
    solve pScope (ExpLetStatement exp) = (pure exp >>= solve pScope) .%% "Solving LetStatement"
    solve pScope (ExpFunctionApplication exp) = (pure exp >>= solve pScope) .%% ("Solving FunctionApplication with scope " ++ show pScope)
    solve pScope (ExpExpressionLookup exp) = (pure exp >>= solve pScope) .%% "Solving ExpressionLookup"
    solve pScope (ExpBuiltInFunc exp) = (pure exp >>= solve pScope) .%% ("Solving BuiltInFunc with scope " ++ show pScope)

    logShow (ExpFunctionContext exp) = logShow exp
    logShow (ExpConst exp) = logShow exp
    logShow (ExpBoolLiteral exp) = logShow exp
    logShow (ExpCharLiteral exp) = logShow exp
    logShow (ExpListLiteral exp) = logShow exp
    logShow (ExpListIndex exp) = logShow exp
    logShow (ExpIfStatement exp) = logShow exp
    logShow (ExpLetStatement exp) = logShow exp
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

parseChar :: Parser Char CharLiteral
parseChar = fmap CharLiteral $ pchar '\'' >>. (pnumber <|> pletter) .>> pchar '\'' where

parseString :: Parser Char ListLiteral
parseString = fmap toString $ pchar '"' >>. many (pnumber <|> pletter <|> whitespace <|> pchar '.') .>> pchar '"' where
    toString = ListLiteral . fmap (ExpCharLiteral . CharLiteral)

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


parseLetStatement :: Parser Char LetStatement
parseLetStatement = fmap toLet $ pseq "let" >> w >>. many1 varExpP .>> pseq "in" .>> w .>>. openExp where
    varExpP = pword .>> w .>> pchar '=' .>> w .>>. openExp .>> w .>> pchar ';' .>> w

    toLet :: ([(String, Expression)], Expression) -> LetStatement
    toLet (boundExps, exp) = LetStatement boundExps exp

parseExpressionLookup :: Parser Char ExpressionLookup
parseExpressionLookup = fmap ExpressionLookup (boolOpp <|> mathOpp <|> variable) where
    variable = pword
    mathOpp = fmap (:"") . anyOf $ "+-*"
    boolOpp = pseq "==" <|> pseq "and" <|> pseq "or" <|> (fmap (:"") . anyOf $ "<>")


singleTerm :: Parser Char Expression
singleTerm = (constant <|> bool <|> char <|> string <|> list <|> ifStatement <|> letStatement <|> funcDecleration <|> expLookup <|> pBrackets singleTerm) <%> "Single Term" where
    constant = fmap ExpConst parseConstant
    bool = fmap ExpBoolLiteral parseBool
    char = fmap ExpCharLiteral parseChar
    string = fmap ExpListLiteral parseString
    list = fmap ExpListLiteral parseList
    ifStatement = fmap ExpIfStatement parseIfStatement
    letStatement = fmap ExpLetStatement parseLetStatement
    funcDecleration = fmap ExpFunctionContext parseFuncDecleration
    expLookup = fmap ExpExpressionLookup parseExpressionLookup


makeFuncApplication :: Expression -> Parser Char Expression
makeFuncApplication e1 = (w >>. parseFuncApply) >>= \e2 -> 
    pure . ExpFunctionApplication . FunctionApplication e1 $ e2 where
        parseFuncApply = pBrackets openExp


makeListIndex :: Expression -> Parser Char Expression
makeListIndex e1 = (w >>. parseListIndex) >>= \e2 -> 
    pure . ExpListIndex . ListIndex e1 $ e2 where
        parseListIndex = pseq "!!" >> w >>. pBrackets openExp 


openExp :: Parser Char Expression
openExp = pConsume cParser singleTerm where
    cParser exp = (makeFuncApplication exp) <|> (makeListIndex exp) 


-- built in functions / stdlib --

crGuard :: (a -> Bool) -> String -> a -> ExecuterT IO a
crGuard f msg x = if f x then pure x else pureFlop msg

onlyTInt :: ComputationResult -> ExecuterT IO Integer
onlyTInt (TInt x) = pure x
onlyTInt other = pureFlop $ "Runtime Exception: Expected Int, got " ++ logShow other

onlyTBool :: ComputationResult -> ExecuterT IO Bool
onlyTBool (TBool x) = pure x
onlyTBool other = pureFlop $ "Runtime Exception: Expected Bool, got " ++ logShow other

onlyTChar :: ComputationResult -> ExecuterT IO Char
onlyTChar (TChar x) = pure x
onlyTChar other = pureFlop $ "Runtime Exception: Expected Char, got " ++ logShow other

onlyString :: ComputationResult -> ExecuterT IO String
onlyString cr = pure cr >>= onlyTList >>= pure . fmap toChar . filter onlyChar where
    onlyChar (TChar x) = True
    onlyChar x = False
    toChar (TChar x) = x

onlyFC :: ComputationResult -> ExecuterT IO FunctionContext
onlyFC (FC x) = pure x
onlyFC other = pureFlop $ "Runtime Exception: Expected FunctionContext, got " ++ logShow other

onlyTList :: ComputationResult -> ExecuterT IO [ComputationResult]
onlyTList (TList x) = pure x
onlyTList other = pureFlop $ "Runtime Exception: Expected List, got " ++ logShow other


twoArgBuiltInFunc :: String -> (ComputationResult -> ComputationResult -> ExecuterT IO ComputationResult) -> BuiltInFunc
twoArgBuiltInFunc name doOpp = BuiltInFunc name f where
    f :: Scope ComputationResult -> ExecuterT IO ComputationResult
    f pScope = mGetOrElse (do
            arg1 <- lookupSymbol "_1" pScope
            arg2 <- lookupSymbol "_2" pScope 
            pure $ doOpp arg1 arg2
        ) (pureFlop $ "Arg lookup failure on built-in func " ++ name) 

builtInAdd :: BuiltInFunc 
builtInAdd = twoArgBuiltInFunc "Add" doOpp where
    doOpp :: ComputationResult -> ComputationResult -> ExecuterT IO ComputationResult
    doOpp cx cy = do
        x <- onlyTInt cx
        y <- onlyTInt cy
        (pure . TInt $ (x + y)) >%%. (\r -> show x ++ " + " ++ show y ++ " = " ++ logShow r)


builtInSub :: BuiltInFunc 
builtInSub = twoArgBuiltInFunc "Sub" doOpp where
    doOpp :: ComputationResult -> ComputationResult -> ExecuterT IO ComputationResult
    doOpp cx cy = do
        x <- onlyTInt cx
        y <- onlyTInt cy
        (pure . TInt $ (x - y)) >%%. (\r -> show x ++ " - " ++ show y ++ " = " ++ logShow r)


builtInMult :: BuiltInFunc 
builtInMult = twoArgBuiltInFunc "Mult" doOpp where
    doOpp :: ComputationResult -> ComputationResult -> ExecuterT IO ComputationResult
    doOpp cx cy = do
        x <- onlyTInt cx
        y <- onlyTInt cy
        (pure . TInt $ (x * y)) >%%. (\r -> show x ++ " * " ++ show y ++ " = " ++ logShow r)


builtInEq :: BuiltInFunc
builtInEq = twoArgBuiltInFunc "Eq" doOpp where
    doOpp :: ComputationResult -> ComputationResult -> ExecuterT IO ComputationResult
    doOpp (TBool x) (TBool y) = (pure . TBool $ (x == y)) >%%. (\r -> show x ++ "==" ++ show y ++ " -> " ++ logShow r)
    doOpp (TInt x) (TInt y) = (pure . TBool $ (x == y)) >%%. (\r -> show x ++ "==" ++ show y ++ " -> " ++ logShow r)
    doOpp (TChar x) (TChar y) = (pure . TBool $ (x == y)) >%%. (\r -> show x ++ "==" ++ show y ++ " -> " ++ logShow r)
    doOpp x y = pureFlop $ "Cannot equate two args of different types: " ++ logShow x ++ " vs " ++ logShow y


builtInGT :: BuiltInFunc
builtInGT = twoArgBuiltInFunc "GT" doOpp where
    doOpp cx cy = do
        x <- onlyTInt cx
        y <- onlyTInt cy
        (pure . TBool $ (x > y)) >%%. (\r -> show x ++ " > " ++ show y ++ " -> " ++ logShow r)

builtInLT :: BuiltInFunc
builtInLT = twoArgBuiltInFunc "LT" doOpp where
    doOpp cx cy = do
        x <- onlyTInt cx
        y <- onlyTInt cy
        (pure . TBool $ (x < y)) >%%. (\r -> show x ++ " < " ++ show y ++ " -> " ++ logShow r)


builtInAnd :: BuiltInFunc
builtInAnd = twoArgBuiltInFunc "And" doOpp where
    doOpp cx cy = do
        x <- onlyTBool cx
        y <- onlyTBool cy
        (pure . TBool $ (x && y)) >%%. (\r -> show x ++ " && " ++ show y ++ " -> " ++ logShow r)

builtInOr :: BuiltInFunc
builtInOr = twoArgBuiltInFunc "Or" doOpp where
    doOpp cx cy = do
        x <- onlyTBool cx
        y <- onlyTBool cy
        (pure . TBool $ (x || y)) >%%. (\r -> show x ++ " || " ++ show y ++ " -> " ++ logShow r)


builtInReadFile :: BuiltInFunc
builtInReadFile = BuiltInFunc "ReadFile" f where

    readFile :: String -> IO String
    readFile fName = do
        handle <- openFile fName ReadMode  
        hGetContents handle

    f :: Scope ComputationResult -> ExecuterT IO ComputationResult
    f pScope = mGetOrElse (do
            crName <- lookupSymbol "_1" pScope
            Just $ (onlyString crName) >>= lift . fmap (TList . fmap TChar) . readFile
        ) (pureFlop $ "Arg lookup failure on built-in func ReadFile") 


builtInPrint :: BuiltInFunc
builtInPrint = BuiltInFunc "Print" f where
    f :: Scope ComputationResult -> ExecuterT IO ComputationResult
    f pScope = mGetOrElse (do
            cr <- lookupSymbol "_1" pScope
            Just $ showAndPassThrough cr
        ) (pureFlop $ "Arg lookup failure on built-in func Print") where

            showAndPassThrough :: ComputationResult -> ExecuterT IO ComputationResult
            showAndPassThrough cr = lift $ (putStr . show $ cr) >> pure cr  


builtInLength :: BuiltInFunc
builtInLength = BuiltInFunc "Length" f where
    f :: Scope ComputationResult -> ExecuterT IO ComputationResult
    f pScope = mGetOrElse (do
            cr <- lookupSymbol "_1" pScope
            Just $ (onlyTList cr) >>= pure . TInt . len
        ) (pureFlop $ "Arg lookup failure on built-in func Length") where
            len ([]) = 0
            len (x:xs) = 1 + len (xs)

builtInSlice :: BuiltInFunc
builtInSlice = BuiltInFunc "Slice" f where
    f :: Scope ComputationResult -> ExecuterT IO ComputationResult
    f pScope = mGetOrElse (do
            start <- lookupSymbol "_1" pScope
            end <- lookupSymbol "_2" pScope
            list <- lookupSymbol "_3" pScope

            Just $ (do
                    i <- onlyTInt start >>= crGuard (>= 0) "Slice start must be >= 0."
                    j <- onlyTInt end >>= crGuard (>= i) "Slice end must be >= than start."
                    l <- onlyTList list
                    (pure . TList $ slice i j l)
                )
        ) (pureFlop $ "Arg lookup failure on built-in func Length") where
            slice :: Integer -> Integer -> [a] -> [a]
            slice i j [] = []
            slice 0 0 xs = []
            slice 0 j (x:xs) = x:(slice 0 (j-1) xs)
            slice i j (x:xs) = slice (i-1) (j-1) xs
            
builtInConcat :: BuiltInFunc
builtInConcat = twoArgBuiltInFunc "Concat" doOpp where
    doOpp cx cy = do
        x <- onlyTList cx
        y <- onlyTList cy
        pure . TList $ (x ++ y)
