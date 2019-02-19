
module Utils
( mGetOrElse
, mOr
, index
, grindex
) where

import Data.Maybe

mGetOrElse :: Maybe s -> s -> s
mGetOrElse (Just x) _ = x
mGetOrElse Nothing y = y

mOr :: Maybe a -> Maybe a -> Maybe a
mOr (Just x) m2 = Just x
mOr (Nothing) m2 = m2

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

