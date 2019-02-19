import Data.Bifunctor
import Data.Maybe
import Data.Either
import Control.Monad
import Control.Monad.Trans

import System.IO  

import Parsing
import Utils
import Scope
import Stack
import RuntimeExpression
import ExecuterT 

-------------------- MAIN EXECUTION --------------------
-- read from file, parse, execute, show --

main = do  
    handle <- openFile "example.suse" ReadMode  
    contents <- hGetContents handle
    putStr . (flip (++)) "\n" . pullInfo . rp parsed . splitNL $ contents
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

splitNL :: String -> [String]
splitNL [] = []
splitNL [x] = [[x]]
splitNL (x:text) = if (x == '\n') then (
        [[x]] ++ rest
    ) else (
        [(x:(head rest))] ++ (tail rest) 
    ) where
    rest = splitNL text 

-------------------- Generic Higher-Order Parser declerations --------------------

pletter :: Parser Char Char
pletter = anyOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

pnumber :: Parser Char Char
pnumber = anyOf "0123456789"

whitespace :: Parser Char Char 
whitespace = pchar ' ' <|> pchar '\t' <|> pchar '\n'

punctuation :: Parser Char Char
punctuation = anyOf ".,'\"!?-~"

pvariableName :: Parser Char String
pvariableName = many1 (pletter <|> pchar '_')

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
    solve scope (Constant x) = (pure . TInt $ x) %%. ("Found " ++ show x)
    logShow (Constant x) = show x

instance Solvable BoolLiteral where
    solve scope (BoolLiteral x) = (pure . TBool $ x) %%. ("Found " ++ show x)
    logShow (BoolLiteral x) = show x

instance Solvable CharLiteral where
    solve scope (CharLiteral x) = (pure . TChar $ x) %%. ("Found " ++ show x)
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
                pure (pure cr %%. ("Binding " ++ logShow cr ++ " to " ++ symbol)) 
            ) (
                pureFlop $ "Symbol " ++ symbol ++ " not found"
            ) .%% ("Looking for " ++ symbol ++ " with scope " ++ show pScope)

    logShow (ExpressionLookup sym) = "ExpLookup(" ++ show sym ++ ")"

instance Solvable BuiltInFunc where
    solve pScope (BuiltInFunc name func) = (func pScope) .%% ("Running built-in func " ++ show name)
    logShow (BuiltInFunc name f) = "BF(" ++ show name ++ ")"

instance Solvable Expression where
    solve pScope (ExpFunctionContext exp) = (pure exp >>= solve pScope) .%% "Solving FunctionContext"
    solve pScope (ExpConst exp) = (pure exp >>= solve pScope) .%% "Solving Constant"
    solve pScope (ExpBoolLiteral exp) = (pure exp >>= solve pScope) .%% "Solving BoolLiteral"
    solve pScope (ExpCharLiteral exp) = (pure exp >>= solve pScope) .%% "Solving CharLiteral"
    solve pScope (ExpListLiteral exp) = (pure exp >>= solve pScope) .%% "Solving ListLiteral"
    solve pScope (ExpListIndex exp) = (pure exp >>= solve pScope) .%% "Solving ListIndex"
    solve pScope (ExpIfStatement exp) = (pure exp >>= solve pScope) .%% "Solving IfStatement"
    solve pScope (ExpLetStatement exp) = (pure exp >>= solve pScope) .%% "Solving LetStatement"
    solve pScope (ExpFunctionApplication exp) = (pure exp >>= solve pScope) .%% "Solving FunctionApplication"
    solve pScope (ExpExpressionLookup exp) = (pure exp >>= solve pScope) .%% "Solving ExpressionLookup"
    solve pScope (ExpBuiltInFunc exp) = (pure exp >>= solve pScope) .%% "Solving BuiltInFunc"

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
parseFuncDecleration = fmap toFuncExp $ pchar '|' >>. pvariableName .>>. pBrackets pvariableName .>>. (w >>. pCurlyBrackets openExp) where
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
    varExpP = pvariableName .>> w .>> pchar '=' .>> w .>>. openExp .>> w .>> pchar ';' .>> w

    toLet :: ([(String, Expression)], Expression) -> LetStatement
    toLet (boundExps, exp) = LetStatement boundExps exp

parseExpressionLookup :: Parser Char ExpressionLookup
parseExpressionLookup = fmap ExpressionLookup (boolOpp <|> mathOpp <|> variable) where
    variable = pvariableName
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
