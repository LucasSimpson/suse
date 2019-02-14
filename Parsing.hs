module Parsing 
( Result (Result, Failure)
, Parser
, rp
, runParser
, (<%>)
, pConsume
, pfor
, pchar
, pand
, (.>>)
, (>>.)
, (.>>.)
, (<|>)
, anyOf
, pseq
, many
, many1
, opt
) where


import Utils
import Data.Bifunctor

-------------------- generic helper dat / funcs --------------------

p1 :: (a, b, c) -> a
p1 (x, y, z) = x
p2 :: (a, b, c) -> b
p2 (x, y, z) = y
p3 :: (a, b, c) -> c
p3 (x, y, z) = z

brackets :: String -> String
brackets s = "(" ++ s ++ ")"

-------------------- Result Monad --------------------

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


-------------------- Parser Monad --------------------


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
rp parser text = runParser parser $ InputState text initPosition

setLabel :: String -> Parser s a -> Parser s a
setLabel newLabel parser = Parser (\text -> 
    case pfunc parser $ text of 
        (Result r) -> Result r
        (Failure (oldLabel, message, pos)) -> Failure (newLabel, message, pos)
    ) newLabel
( <%> ) = flip setLabel

-------------------- Parser Combinators --------------------

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
    
_f = \x -> fmap (\z -> x * 2) (pchar 'd')
_m = fmap (\z -> 10) $ pchar 'c'

n = pConsume _f _m 

pConsume :: (a -> Parser s a) -> Parser s a -> Parser s a
pConsume f firstParser = Parser parseFunc (label firstParser) where
        parseFunc inputState = passThroughOrDefault inputState $ newResult inputState
        newResult inputState = runParser (firstParser >>= f) inputState
        passThroughOrDefault inputState (Result (res, newState)) = runParser (pConsume f $ pure res) newState 
        passThroughOrDefault inputState (Failure err) = runParser firstParser inputState


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

