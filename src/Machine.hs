{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell, RankNTypes, FlexibleContexts, FlexibleInstances, UndecidableInstances, MultiWayIf, TypeFamilies #-}
module Machine
  ( Machine
  , createNamedMachine
  , createDebugMachine
  , createMachine
  , machineName
  , rawMachine
  , traceEvent
  , NamedMachine
  , DebugMachine
  , liftNamedMachine
  , unwrapST
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
import Debug.Trace (traceM)

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


type MachineM ev m  = Coroutine (Request (String, [ev]) ev) m -- Request out@(name, send) in
type RawMachine ev m a = ev -> MachineM ev m a

data NamedMachine ev m a = NamedMachine
  { _nMachineName :: String
  , _nMachine :: RawMachine ev m a
  }

makeLenses ''NamedMachine

newtype DebugMachine ev m a = DebugMachine (NamedMachine ev m a)

makeWrapped ''DebugMachine

createNamedMachine = NamedMachine
createDebugMachine x y = DebugMachine $ NamedMachine x y

class Machine mc where
    createMachine :: String -> RawMachine ev m a -> mc ev m a
    machineName   :: Lens' (mc ev m a) String
    rawMachine    :: Lens' (mc ev m a) (RawMachine ev m a)
    traceEvent    :: (Monad m) => mc ev m a -> String -> m ()

instance Machine NamedMachine where
    createMachine  = createNamedMachine
    machineName    = nMachineName
    rawMachine     = nMachine
    traceEvent _ _ = return ()

instance Machine DebugMachine where
    createMachine  = createDebugMachine
    machineName    = _Wrapped . nMachineName
    rawMachine     = _Wrapped . nMachine
    traceEvent _ s = traceM s


trans1 :: (Show ev, MonadError ev m, Machine mc) => mc ev m a -> ev -> m (Either ([ev], mc ev m a) a)
trans1 mc ev = do
        m `catchError` \x -> do
            traceEvent mc $ "fail!! " <> show x <> " : ( " <> mn <> " )"
            throwError x
        where
            mn = mc ^. machineName
            ml = mc ^. rawMachine
            m = do
                traceEvent mc $ "test   " <> show ev <> " : ( " <> mn <> " )"
                ret <- (fmap $ first unwrapRequest) $ resume $ ml ev
                let news = either (\l -> " out: " <> (show $ fst l)) (const "") ret
                traceEvent mc $ "trans! " <> show ev <> " : ( " <> mn <> " )" <> news
                return ret
            unwrapRequest (Request (mn', reqs) ml) = (reqs, createMachine (if null mn' then mn else mn') ml)


liftNamedMachine :: (MonadTrans t, Monad m, Machine mc) => mc ev m a -> mc ev (t m) a
liftNamedMachine nm = createMachine n m'
    where
        m = nm ^. rawMachine
        n = nm ^. machineName
        m' = convM lift . m

unwrapST :: (Monad m, Machine mc) => s -> mc ev (StateT s m) a -> mc ev m a
unwrapST s nm = createMachine n m'
    where
        m  = nm ^. rawMachine
        n  = nm ^. machineName
        m' = go s . m
        go :: (Functor f, Monad m) => s -> Coroutine f (StateT s m) a -> Coroutine f m a
        go s c@(Coroutine m) = Coroutine $ do
            (result, s') <- runStateT m s -- m (s, Either (f (Coroutine f m r)) r)
            let result' = (first $ fmap $ go s') $ result
            return result'

trans :: (Show ev, MonadError ev m, Machine mc) => mc ev m [ev] -> [ev] -> m (Maybe (mc ev m [ev]))
trans nm [] = return $ Just nm
trans nm (ev:evs) = do
    traceEvent nm $ "\n** start ** " <> show ev <> " : remain ( " <> show evs <> " )"
    result <- trans1 nm ev
    traceEvent nm $ "** done **"
    case result of
        Left (_, nm') -> trans nm' evs
        Right _ -> if null evs
            then return $ Nothing
            else throwError $ head evs


batchTrans :: (Show ev, Eq ev, MonadError ev m, Machine mc) => mc ev m [ev] -> [ev] -> m (Maybe (mc ev m [ev]))
batchTrans nm [] = return $ Just nm
batchTrans nm (ev:evs) = do
    traceEvent nm $ "\n** start ** " <> show ev <> " : remain ( " <> show evs <> " )"
    result <- trans1 nm ev
    traceEvent nm $ "** done **"
    case result of
        Left (reqs, nm') -> batchTrans nm' $ nub $ reqs <> evs
        Right reqs -> if null $ nub $ reqs <> evs
            then return $ Nothing
            else throwError ev

batchTransSkipError :: (Show ev, Eq ev, MonadError ev m, Machine mc) => mc ev m [ev] -> [ev] -> m (Maybe (mc ev m [ev]))
batchTransSkipError nm [] = return $ Just nm
batchTransSkipError nm (ev:evs) = do
    traceEvent nm $ "\n** start ** "
    traceEvent nm $ "proc  : " <> nm ^. machineName
    traceEvent nm $ "event : " <> show ev <> " : remain ( " <> show evs <> " )"
    result <- (do
        result <- trans1 nm ev
        traceEvent nm $ "done"
        return result
        ) `catchError` (\x -> do
            traceEvent nm "skip"
            return $ Left ([], nm)
            )
    case result of
        Left (reqs, nm') -> do
            traceEvent nm $ "result : " <> nm' ^. machineName
            batchTransSkipError nm' $ nub $ reqs <> evs
        Right reqs -> if null $ nub $ reqs <> evs
            then return $ Nothing
            else throwError ev

stop :: (MonadError ev m, Machine mc) => mc ev m a
stop = createMachine "STOP" throwError

--------------------------
-- prefix
--------------------------

recv :: (MonadError ev m) => (ev -> Maybe b) -> MachineM ev m b
recv p = do
    ev <- request ("", [])
    case p ev of
        Just a -> return a
        Nothing -> throwError ev

sendOr :: (Monad m, Eq ev) => ev -> RawMachine ev m [ev] -> MachineM ev m [ev]
sendOr ev ml = do
    ev' <- request ("", [ev])
    if ev == ev'
        then return []
        else ml ev'

send :: (Show ev, MonadError ev m, Eq ev) => ev -> MachineM ev m ()
send ev = do
    sendOr ev throwError
    return ()


(|=>) :: (Monad m, Machine mc) => [ev] -> mc ev m a -> MachineM ev m a
req |=> mc = request (mn, req) >>= ml
    where
        mn = mc ^. machineName
        ml = mc ^. rawMachine
infixr 0 |=>

(!&) :: (MonadError ev m) => Bool -> ev -> MachineM ev m ()
p !& ev = when (not p) $ throwError ev

infixr 0 !&

(?&) :: (MonadError ev m) => Maybe a -> ev -> MachineM ev m a
m ?& ev = case m of
    Just a -> return a
    Nothing -> throwError ev

infixr 0 ?&
continueTo :: (Monad m, Machine mc) => mc ev m a -> MachineM ev m a
continueTo mc = request (mn, []) >>= ml
    where
        mn = mc ^. machineName
        ml = mc ^. rawMachine

--------------------------
-- composition
--------------------------

(>>>) :: (Monad m, Machine mc) => mc ev m [ev] -> mc ev m [ev] -> mc ev m [ev]
mc >>> nm = createMachine fn $ \ev -> do
        out <- f ev
        out |=> nm
    where
        fn = mc ^. machineName
        f  = mc ^. rawMachine
infixr 0 >>>

(|=|) :: (MonadError ev m, Machine mc) => mc ev m a -> mc ev m a -> mc ev m a
mcf  |=| mcg = createMachine (fn <> " [] " <> gn) $ \x -> (mapFirstSuspension (rename fn) $ f x) `catchFirstError` (\_ -> mapFirstSuspension (rename gn) $ g x)
    where
        fn = mcf ^. machineName
        f  = mcf ^. rawMachine 
        gn = mcg ^. machineName
        g  = mcg ^. rawMachine 
infixr 0 |=|


(\/) :: (MonadError ev m, Machine mc) => mc ev m a -> mc ev m a -> mc ev m a
mcf \/ mcg = createMachine (fn <> " \\/ " <> gn) $ \x -> Coroutine $ do
    resume (mapFirstSuspension (rename gn) $ g x) `catchError` (\_ -> do
        result <- resume $ f x
        case result of
            Left (Request (fn', reqs) f') -> return $ Left $ Request (fn' <> " \\/ " <> gn, reqs) $ ((createMachine fn' f') \/ mcg) ^. rawMachine
            Right reqs -> return $ Right reqs
        )
    where
        fn  = mcf ^. machineName
        f   = mcf ^. rawMachine 
        gn  = mcg ^. machineName
        g   = mcg ^. rawMachine 


interfaceP :: (Show ev, Eq ev, MonadError ev m, Machine mc) => (ev -> Bool) -> mc ev m [ev] -> mc ev m [ev] -> mc ev m [ev]
interfaceP p mcf mcg = createMachine (fn <> " [|I|] " <> gn) $ \x ->
    if p x
        then goBoth (interfaceP p) mcf mcg x
        else goEither (interfaceP p) mcf mcg x
    where
        fn  = mcf ^. machineName
        gn  = mcg ^. machineName

(|||) :: (Show ev, Eq ev, MonadError ev m, Machine mc) => mc ev m [ev] -> mc ev m [ev] -> mc ev m [ev]
(|||) f g = interfaceP (const False) f g

infixr 0 |||


-- alphabet parallel
(<||>) :: (Eq ev, Show ev, MonadError ev m, Machine mc) => ((ev -> Bool), mc ev m [ev]) -> ((ev -> Bool), mc ev m [ev]) -> mc ev m [ev]
(pf, f) <||> (pg, g) = createMachine (f ^. machineName <> " [|A|] " <> g ^. machineName) $ \x -> if
    | pf x && pg x -> goBoth  op f g x
    | pf x         -> goLeft  op f g x
    | pg x         -> goRight op f g x
    | otherwise -> throwError x
    where
    op f g = (pf, f) <||> (pg, g)

(|.|) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
f |.| g = \x -> f x || g x

alphabetP' :: (Eq ev, Show ev, MonadError ev m, Machine mc) => [((ev -> Bool), mc ev m [ev])] -> mc ev m [ev]
alphabetP' [] = stop -- any p [] == False
alphabetP' ((p, m):[]) = error "not implimented" -- hide is needed
alphabetP' (m1:m2:[]) = m1 <||> m2
alphabetP' (m:ms) = m <||> (\ev -> any ($ ev) (ms ^.. traverse . _1), createMachine (concat $ intersperse " [|A|] " (ms ^.. traverse . _2 . machineName)) $ (alphabetP' ms) ^. rawMachine)

goBoth :: (Eq ev, Show ev, MonadError ev m, Machine mc)
    => (mc ev m [ev] -> mc ev m [ev] -> mc ev m [ev])
    -> mc ev m [ev] -> mc ev m [ev] -> RawMachine ev m [ev]
goBoth op mcf mcg x = Coroutine $ do
    result <- trans1 mcf x
    case result of
        Left (reqs, mcf') -> do
            result <- trans1 mcg x
            case result of
                Left (reqs', mcg') -> do
                    let mc = mcf' `op` mcg'
                    return $ Left $ Request (mc ^. machineName, nub $ reqs <> reqs') $ mc ^. rawMachine
                Right reqs' -> return $ Left $ Request (mcf' ^. machineName, nub $ reqs <> reqs') $ mcf' ^. rawMachine
        Right reqs -> do
            result <- trans1 mcg x
            case result of
                Left (reqs', mcg') -> return $ Left $ Request (mcg' ^. machineName, nub $ reqs <> reqs') $ mcg' ^. rawMachine
                Right reqs' -> return $ Right $ nub $ reqs <> reqs'


goLeft :: (Eq ev, Show ev, MonadError ev m, Machine mc)
    => (mc ev m [ev] -> mc ev m [ev] -> mc ev m [ev])
    -> mc ev m [ev] -> mc ev m [ev] -> RawMachine ev m [ev]
goLeft op mcf mcg = \x -> Coroutine $ do
    result <- trans1 mcf x
    case result of
        Left (reqs, mcf') -> do
            let mc = mcf' `op` mcg
            return $ Left $ Request (mc ^. machineName, reqs) $ mc ^. rawMachine
        Right reqs -> return $ Left $ Request (mcg ^. machineName, reqs) $ mcg ^. rawMachine

goRight :: (Eq ev, Show ev, MonadError ev m, Machine mc)
    => (mc ev m [ev] -> mc ev m [ev] -> mc ev m [ev])
    -> mc ev m [ev] -> mc ev m [ev] -> RawMachine ev m [ev]
goRight op mcf mcg = \x -> Coroutine $ do
    result <- trans1 mcg x
    case result of
        Left (reqs, mcg') -> do
            let mc = mcf `op` mcg'
            return $ Left $ Request (mc ^. machineName, reqs) $ mc ^. rawMachine
        Right reqs -> return $ Left $ Request (mcf ^. machineName, reqs) $ mcf ^. rawMachine

goEither :: (Eq ev, Show ev, MonadError ev m, Machine mc)
    => (mc ev m [ev] -> mc ev m [ev] -> mc ev m [ev])
    -> mc ev m [ev] -> mc ev m [ev] -> RawMachine ev m [ev]
goEither op f g =  (ml |=| mr) ^. rawMachine
    where
        ml = (createMachine "" $ goLeft  op f g) `asTypeOf` g
        mr = (createMachine "" $ goRight op f g) `asTypeOf` g

