module ExecuterT 
( ExecuterT
, runExecuterT
, pureFlop
, display    
, (%%.)
, (.%%)
, (>%%.)
, (>.%%)
) where

import Control.Monad
import Control.Monad.Trans


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
                addLogs (Executer logs x) = pure $ Executer (logs ++ ll) x

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
( %%. ) = appendLogT

prependLogT :: ExecuterT IO a -> String -> ExecuterT IO a
prependLogT = liftET2 prependLog
( .%% ) = prependLogT

appendContextLogT :: ExecuterT IO a -> (a -> String) -> ExecuterT IO a
appendContextLogT = liftET2 appendContextLog
( >%%. ) = appendContextLogT


prependContextLogT :: ExecuterT IO a -> (a -> String) -> ExecuterT IO a
prependContextLogT = liftET2 prependContextLog
( >.%% ) = prependContextLogT


