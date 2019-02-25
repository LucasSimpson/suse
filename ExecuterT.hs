module ExecuterT 
( ExecuterT
, runExecuterT
, pureFlop
, display    
, (%%.)
, (.%%)
, (>%%.)
) where

import Control.Monad
import Control.Monad.Trans


newtype Executer a = Executer { solvedResult :: Either String a }

instance Functor Executer where
    fmap f (Executer result) = Executer $ fmap f result

instance Applicative Executer where
    pure x = Executer $ pure x
    (Executer r1) <*> (Executer r2) = Executer (r1 <*> r2)

instance Monad Executer where
    return = pure
    Executer (Left msg) >>= f = Executer $ Left msg
    Executer (Right x) >>= f = Executer . solvedResult $ f x


instance (Show a) => Show (Executer a) where
    show (Executer r) = "Executer "  ++ (show r) where


executerFlop :: String -> Executer a
executerFlop msg = Executer $ Left msg


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
            (Executer (Left msg)) -> return . executerFlop $ msg
            (Executer (Right x))  -> (runExecuterT $ f x)

instance MonadTrans ExecuterT where
    lift x = ExecuterT $ fmap pure x

display :: (Show a) => IO (Executer a) -> IO ()
display iox = do
    exec <- iox
    putStr . show $ exec
    putChar '\n'


pureFlop :: String -> ExecuterT IO a
pureFlop msg = ExecuterT . return . executerFlop $ msg

traceLog :: ExecuterT IO a -> String -> ExecuterT IO a
traceLog exec msg = exec >>= lift . f where
    f :: a -> IO a
    f = \x -> do
        putStr msg
        putChar '\n'
        return x 
( %%. ) = traceLog

traceLogBefore :: ExecuterT IO a -> String -> ExecuterT IO a
traceLogBefore exec msg = traceLog (pure ()) msg >>= c exec where
    c r _ = r
( .%% ) = traceLogBefore

traceLogContext :: ExecuterT IO a -> (a -> String) -> ExecuterT IO a
traceLogContext exec f = do
    r <- exec
    traceLog (pure r) $ f r
( >%%. ) = traceLogContext


