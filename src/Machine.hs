{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell, RankNTypes, FlexibleContexts, FlexibleInstances, UndecidableInstances, MultiWayIf, CPP #-}
-- #define DEBUG
module Machine
  ( Machine
  , MachineM
  , NamedMachine(..)
  , liftNamedMachine
  , machine
  , machineName
  , unwrapST
  , unwrapR
  , convM
  , trans1
  , trans
  , batchTrans
  , batchTransSkipError
  , recv
  , send
  , sendOr
  , continueTo
  , (|.|)
  , (|=>)
  , (!&)
  , (?&)
  , (>>>)
  , (|=|)
  , (\/)
  , interfaceP
  , (|||)
  , (<||>)
  , alphabetP'
  , MonadIO
  , MonadState
  , MonadError
  , MonadReader
  , MonadWriter
  , lift
  ) where

import Data.Maybe(fromMaybe)
import Data.Monoid((<>))
import Data.List(nub, intersperse)
import Data.Bifunctor
import Control.Applicative
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors(Request(Request), request)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.Trans.Class (lift)
import Data.Either(fromLeft)
import Control.Lens
#ifdef DEBUG
import Debug.Trace (traceM)
#else
traceM :: (Monad m) => String -> m ()
--traceM :: (MonadWriter [String] m) => String -> m ()
traceM _ = return ()

#endif

type MachineM ev m  = Coroutine (Request (String, [ev]) ev) m -- Request out@(name, send) in
type Machine ev m a = ev -> MachineM ev m a

data NamedMachine ev m a = NamedMachine
  { _machineName :: String
    , _machine :: Machine ev m a
  }

makeLenses ''NamedMachine

rename :: String -> Request (String, a) b c -> Request (String, a) b c
rename s (Request (_, a) b) = Request (s, a) b

instance (Functor f, MonadReader r m) => MonadReader r (Coroutine f m) where
    ask = lift ask
    local f m = undefined

instance (Functor f, MonadWriter w m) => MonadWriter w (Coroutine f m) where
    writer = lift . writer
    tell   = lift . tell
    listen = undefined
    pass   = undefined

instance (Functor f, MonadState s m) => MonadState s (Coroutine f m) where
    get = lift get
    put = lift . put
    state = lift . state

instance (Functor f, MonadError e m) => MonadError e (Coroutine f m) where
    throwError = lift . throwError
    catchError m h = Coroutine $ do
        (do
            result <- resume m
            case result of
                Left f -> return $ Left $ fmap (flip catchError h) f
                Right r -> return $ Right r
            ) `catchError` (\e -> resume $ h e)

catchFirstError :: (Functor f, MonadError e m) => Coroutine f m a -> (e -> Coroutine f m a) -> Coroutine f m a
catchFirstError m h = Coroutine $ resume m `catchError` (\e -> resume $ h e)

convM :: (Functor f, Functor m) => (forall b. m b -> m' b) -> Coroutine f m a -> Coroutine f m' a
convM g (Coroutine m) = Coroutine $ g $ (fmap $ first $ fmap $ convM g) m

liftNamedMachine :: (MonadTrans t, Monad m) => NamedMachine ev m a -> NamedMachine ev (t m) a
liftNamedMachine nm = nm & machine %~ (convM lift .)

unwrapST :: (Monad m') => s -> NamedMachine ev (StateT s m') a -> NamedMachine ev m' a
unwrapST s nm = nm & machine %~ (go s . )
    where
        go :: (Functor f, Monad m') => s -> Coroutine f (StateT s m') a -> Coroutine f m' a
        go s c@(Coroutine m) = Coroutine $ do
            (result, s') <- runStateT m s -- m (s, Either (f (Coroutine f m r)) r)
            let result' = (first $ fmap $ go s') $ result
            return result'

unwrapR :: (Monad m') => r -> NamedMachine ev (ReaderT r m') a -> NamedMachine ev m' a
unwrapR s nm = nm & machine %~ (convM (flip runReaderT s) .)


trans1 :: (Show ev, MonadError ev m) => NamedMachine ev m a -> ev -> m (Either ([ev], NamedMachine ev m a) a)
trans1 (NamedMachine mn ml) ev = do
    m `catchError` \x -> do
        traceM ("fail!! " <> show x <> " : ( " <> mn <> " )")
        throwError x
    where
        m = do
            traceM $ "test   " <> show ev <> " : ( " <> mn <> " )"
            ret <- (fmap $ first unwrapRequest) $ resume $ ml ev
            let news = either (\l -> " out: " <> (show $ fst l)) (const "") ret
            traceM $ "trans! " <> show ev <> " : ( " <> mn <> " )" <> news
            return ret
        unwrapRequest (Request (mn', reqs) ml) = (reqs, NamedMachine (if null mn' then mn else mn') ml)


trans :: (Show ev, MonadError ev m, Monad m) => NamedMachine ev m [ev] -> [ev] -> m (Maybe (NamedMachine ev m [ev]))
trans nm [] = return $ Just nm
trans nm (ev:evs) = do
    traceM ("\n** start ** " <> show ev <> " : remain ( " <> show evs <> " )")
    result <- trans1 nm ev
    traceM ("** done **")
    case result of
        Left (_, nm') -> trans nm' evs
        Right _ -> if null evs
            then return $ Nothing
            else throwError $ head evs


batchTrans :: (Show ev, MonadError ev m, Monad m, Eq ev) => NamedMachine ev m [ev] -> [ev] -> m (Maybe (NamedMachine ev m [ev]))
batchTrans nm [] = return $ Just nm
batchTrans nm (ev:evs) = do
    traceM ("\n** start ** " <> show ev <> " : remain ( " <> show evs <> " )")
    result <- trans1 nm ev
    traceM ("** done **")
    case result of
        Left (reqs, nm') -> batchTrans nm' $ nub $ reqs <> evs
        Right reqs -> if null $ nub $ reqs <> evs
            then return $ Nothing
            else throwError ev

batchTransSkipError :: (Show ev, MonadError ev m, Monad m, Eq ev) => NamedMachine ev m [ev] -> [ev] -> m (Maybe (NamedMachine ev m [ev]))
batchTransSkipError nm [] = return $ Just nm
batchTransSkipError nm (ev:evs) = do
    traceM "\n** start ** "
    traceM $ "proc  : " <> nm ^. machineName
    traceM $ "event : " <> show ev <> " : remain ( " <> show evs <> " )"
    result <- (do
        result <- trans1 nm ev
        traceM ("done")
        return result
        ) `catchError` (\x -> do
            traceM "skip"
            return $ Left ([], nm)
            )
    case result of
        Left (reqs, nm') -> do
            traceM $ "result : " <> nm' ^. machineName
            batchTransSkipError nm' $ nub $ reqs <> evs
        Right reqs -> if null $ nub $ reqs <> evs
            then return $ Nothing
            else throwError ev



stop :: (MonadError ev m) => NamedMachine ev m a
stop = NamedMachine "STOP" throwError

--------------------------
-- prefix
--------------------------

recv :: (MonadError ev m) => (ev -> Maybe b) -> MachineM ev m b
recv p = do
    ev <- request ("", [])
    case p ev of
        Just a -> return a
        Nothing -> throwError ev

sendOr :: (Monad m, Eq ev) => ev -> Machine ev m [ev] -> MachineM ev m [ev]
sendOr ev ml = do
    ev' <- request ("", [ev])
    if ev == ev'
        then return []
        else ml ev'

send :: (Show ev, MonadError ev m, Eq ev) => ev -> MachineM ev m ()
send ev = do
    sendOr ev throwError
    return ()


(|=>) :: (Monad m) => [ev] -> NamedMachine ev m a -> MachineM ev m a
req |=> (NamedMachine mn ml) = request (mn, req) >>= ml
infixr 0 |=>

(!&) :: (MonadError ev m) => Bool -> ev -> MachineM ev m ()
p !& ev = when (not p) $ throwError ev

infixr 0 !&

(?&) :: (MonadError ev m) => Maybe a -> ev -> MachineM ev m a
m ?& ev = case m of
    Just a -> return a
    Nothing -> throwError ev

infixr 0 ?&
continueTo :: (Monad m) => NamedMachine ev m a -> MachineM ev m a
continueTo (NamedMachine mn ml) = request (mn, []) >>= ml

--------------------------
-- composition
--------------------------

(>>>) :: (Monad m) => NamedMachine ev m [ev] -> NamedMachine ev m [ev] -> NamedMachine ev m [ev]
(NamedMachine fn f) >>> nm = NamedMachine fn $ \ev -> do
        out <- f ev
        out |=> nm
infixr 0 >>>

(|=|) :: (MonadError ev m) => NamedMachine ev m a -> NamedMachine ev m a -> NamedMachine ev m a
(NamedMachine fn f) |=| (NamedMachine gn g) = NamedMachine (fn <> " [] " <> gn) $ \x -> (mapFirstSuspension (rename fn) $ f x) `catchFirstError` (\_ -> mapFirstSuspension (rename gn) $ g x)
infixr 0 |=|


(\/) :: (MonadError ev m) => NamedMachine ev m a -> NamedMachine ev m a -> NamedMachine ev m a
(NamedMachine fn f) \/ (NamedMachine gn g) = NamedMachine (fn <> " \\/ " <> gn) $ \x -> Coroutine $ do
    resume (mapFirstSuspension (rename gn) $ g x) `catchError` (\_ -> do
        result <- resume $ f x
        case result of
            Left (Request (fn', reqs) f') -> return $ Left $ Request (fn' <> " \\/ " <> gn, reqs) $ ((NamedMachine fn' f') \/ (NamedMachine gn g)) ^. machine
            Right reqs -> return $ Right reqs
        )

interfaceP :: (Show ev, Eq ev, MonadError ev m) => (ev -> Bool) -> NamedMachine ev m [ev] -> NamedMachine ev m [ev] -> NamedMachine ev m [ev]
interfaceP p (NamedMachine fn f) (NamedMachine gn g) = NamedMachine (fn <> " [|I|] " <> gn) $ \x ->
    if p x
        then goBoth (interfaceP p) (NamedMachine fn f) (NamedMachine gn g) x
        else goEither (interfaceP p) (NamedMachine fn f) (NamedMachine gn g) x

(|||) :: (Show ev, Eq ev, MonadError ev m) => NamedMachine ev m [ev] -> NamedMachine ev m [ev] -> NamedMachine ev m [ev]
(|||) f g = interfaceP (const False) f g

infixr 0 |||


-- alphabet parallel
(<||>) :: (Eq ev, Show ev, MonadError ev m) => ((ev -> Bool), NamedMachine ev m [ev]) -> ((ev -> Bool), NamedMachine ev m [ev]) -> NamedMachine ev m [ev]
(pf, f) <||> (pg, g) = NamedMachine (f ^. machineName <> " [|A|] " <> g ^. machineName) $ \x -> if
    | pf x && pg x -> goBoth  op f g x
    | pf x         -> goLeft  op f g x
    | pg x         -> goRight op f g x
    | otherwise -> throwError x
    where
    op f g = (pf, f) <||> (pg, g)

(|.|) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
f |.| g = \x -> f x || g x

alphabetP' :: (Eq ev, Show ev, MonadError ev m) => [((ev -> Bool), NamedMachine ev m [ev])] -> NamedMachine ev m [ev]
alphabetP' [] = stop -- any p [] == False
alphabetP' ((p, m):[]) = error "not implimented" -- hide is needed
alphabetP' (m1:m2:[]) = m1 <||> m2
alphabetP' (m:ms) = m <||> (\ev -> any ($ ev) (ms ^.. traverse . _1), NamedMachine (concat $ intersperse " [|A|] " (ms ^.. traverse . _2 . machineName)) $ (alphabetP' ms) ^. machine)

goBoth :: (Eq ev, Show ev, MonadError ev m)
    => (NamedMachine ev m [ev] -> NamedMachine ev m [ev] -> NamedMachine ev m [ev])
    -> NamedMachine ev m [ev] -> NamedMachine ev m [ev] -> Machine ev m [ev]
goBoth op (NamedMachine fn f) (NamedMachine gn g) x = Coroutine $ do
    --when (not $ null fn) $ traceM $ "inBL: " <> show x <> "@" <> fn
    result <- trans1 (NamedMachine fn f) x
    case result of
        Left (reqs, (NamedMachine fn' f')) -> do
            --when (not $ null gn) $ traceM $ "inBR: " <> show x <> "@" <> gn
            result <- trans1 (NamedMachine gn g) x
            case result of
                Left (reqs', (NamedMachine gn' g')) -> do
                    let (NamedMachine mn ml) = (NamedMachine fn' f') `op` (NamedMachine gn' g')
                    return $ Left $ Request (mn, nub $ reqs <> reqs') $ ml
                Right reqs' -> return $ Left $ Request (fn', nub $ reqs <> reqs') f'
        Right reqs -> do
            result <- trans1 (NamedMachine gn g) x
            case result of
                Left (reqs', (NamedMachine gn' g')) -> return $ Left $ Request (gn', nub $ reqs <> reqs') $ g'
                Right reqs' -> return $ Right $ nub $ reqs <> reqs'

goLeft :: (Eq ev, Show ev, MonadError ev m)
    => (NamedMachine ev m [ev] -> NamedMachine ev m [ev] -> NamedMachine ev m [ev])
    -> NamedMachine ev m [ev] -> NamedMachine ev m [ev] -> Machine ev m [ev]
goLeft op (NamedMachine fn f) (NamedMachine gn g) = \x -> Coroutine $ do
    --when (not $ null fn) $ traceM $ "inL: " <> show x <> "@" <> fn
    result <- trans1 (NamedMachine fn f) x
    case result of
        Left (reqs, (NamedMachine fn' f')) -> do
            let (NamedMachine mn ml) = (NamedMachine fn' f') `op` (NamedMachine gn g)
            return $ Left $ Request (mn, reqs) $ ml
        Right reqs -> return $ Left $ Request (gn, reqs) g

goRight :: (Eq ev, Show ev, MonadError ev m)
    => (NamedMachine ev m [ev] -> NamedMachine ev m [ev] -> NamedMachine ev m [ev])
    -> NamedMachine ev m [ev] -> NamedMachine ev m [ev] -> Machine ev m [ev]
goRight op (NamedMachine fn f) (NamedMachine gn g) = \x -> Coroutine $ do
    --when (not $ null gn) $ traceM $ "inR: " <> show x <> "@" <> gn
    result <- trans1 (NamedMachine gn g) x
    case result of
        Left (reqs, (NamedMachine gn' g')) -> do
            let (NamedMachine mn ml) = (NamedMachine fn f) `op` (NamedMachine gn' g')
            return $ Left $ Request (mn, reqs) $ ml
        Right reqs -> return $ Left $ Request (fn, reqs) f

goEither :: (Eq ev, Show ev, MonadError ev m)
    => (NamedMachine ev m [ev] -> NamedMachine ev m [ev] -> NamedMachine ev m [ev])
    -> NamedMachine ev m [ev] -> NamedMachine ev m [ev] -> Machine ev m [ev]
goEither op f g =  ((NamedMachine "" $ goLeft op f g) |=| (NamedMachine "" $ goRight op f g)) ^. machine

