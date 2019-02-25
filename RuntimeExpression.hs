module RuntimeExpression 
( Constant (Constant)
, BoolLiteral (BoolLiteral)
, CharLiteral (CharLiteral)
, ListLiteral (ListLiteral)
, ListIndex (ListIndex)
, FunctionContext (FunctionContext)
, BuiltInFunc (BuiltInFunc)
, ExpressionLookup (ExpressionLookup)
, FunctionApplication (FunctionApplication)
, IfStatement (IfStatement)
, LetStatement (LetStatement)
, Expression (  ExpFunctionContext,  
                ExpConst,
                ExpBoolLiteral,
                ExpCharLiteral,
                ExpListLiteral,
                ExpListIndex,
                ExpIfStatement,
                ExpLetStatement,
                ExpFunctionApplication,
                ExpExpressionLookup,
                ExpBuiltInFunc  )

, ComputationResult (   TInt,
                        TBool,
                        TChar,
                        TList,
                        FC  )
) where

import Scope
import Stack
import ExecuterT


newtype Constant = Constant Integer deriving Show
newtype BoolLiteral = BoolLiteral Bool deriving Show
newtype CharLiteral = CharLiteral Char deriving Show
newtype ListLiteral = ListLiteral [Expression] deriving Show

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
