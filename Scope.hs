module Scope 
( Symbol
, SymbolTable
, Scope
, newScope
, setParent
, lookupSymbol
, bindToScope
) where

import Utils

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
