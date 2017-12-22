{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell, FlexibleContexts,MultiWayIf #-}

import Machine
import Data.Monoid
import Control.Lens
import Control.Monad.State
import Control.Monad.Except

--------------------------------------------------------------
-- App: 自動販売機
--------------------------------------------------------------

data Ev   = Coin | Juice | Check | Fill Int | Trouble | Repair deriving (Eq, Show)

makePrisms ''Ev


trouble :: (MonadIO m, MonadError Ev m, MonadState Int m) => NamedMachine Ev m [Ev]
trouble = NamedMachine "trouble" $ \ev -> case ev of
    Trouble -> do
        recv (^? _Repair)
        continueTo bvm
    _ -> throwError ev

bvm :: (MonadIO m, MonadError Ev m, MonadState Int m) => NamedMachine Ev m [Ev]
bvm = NamedMachine "bvm" $ \ev -> case ev of
    Coin -> do
        n <- get
        n > 0 !& ev
        recv (^? _Juice)
        put $ n - 1
        continueTo bvm
    Check -> do
        n <- get
        --liftIO $ print n
        when (n < 5) $ do
            send (Fill (10 - n))
            put $ 10
        continueTo bvm
    _ -> throwError ev

type M = StateT Int (ExceptT Ev IO)

runM :: Int -> M a -> IO ()
runM x m = do
    result <- runExceptT $ runStateT m x
    case result of
        Left ev -> putStrLn $ "error:" <> show ev
        Right (_, n) -> putStrLn $ "result: " <> show n

test :: NamedMachine Ev M [Ev] -> M ()
test m = do
    Left (_, m) <- trans1 m Coin
    Left (_, m) <- trans1 m Juice
    Left ([Fill n], m) <- trans1 m Check
    Left (_, m) <- trans1 m $ Fill n
    Left (_, m) <- trans1 m Coin
    Left (_, m) <- trans1 m Juice
    Left (_, m) <- trans1 m Trouble
    Left (_, m) <- trans1 m Repair
    Left (_, m) <- trans1 m Coin
    Left (_, m) <- trans1 m Juice
    return ()

main :: IO ()
main = do
    let m = bvm \/ trouble
    print =<< (runM 5 $ test $ m)

