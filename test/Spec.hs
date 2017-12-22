{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell, FlexibleContexts,MultiWayIf, RankNTypes #-}

import Machine
import Control.Lens
import Control.Monad.Writer
import Control.Monad.Except

import Test.Hspec
import Test.Hspec.QuickCheck (prop)

-- TODO
-- 遷移則をと受かってQuickCheckか何かで網羅的にやれる仕組みが欲しい(木をGenする方法でマシンもGen)

data Ev   = A Int | B Int | C Int | D Int deriving (Show, Eq)
makePrisms ''Ev

type M = WriterT [Ev] (ExceptT Ev Identity)

isAccepted :: M a -> Bool
isAccepted m = has _Right $ runIdentity $ runExceptT $ runWriterT $ m

run :: M a -> [Ev]
run m = case runIdentity $ runExceptT $ runWriterT $ m of
    Left _ -> []
    Right (_, w) -> w

bm1 :: (MonadError Ev m) => NamedMachine Ev m [Ev]
bm1 = NamedMachine "bm1" $ \ev -> case ev of
    A n -> do
        n' <- recv (^? _A)
        n == n' !& A n'
        send (B 1)
        recv (^? _C)
        continueTo bm1
    C n -> if n == 0
            then do
              recv (^? _B)
              return []
            else [C $ n - 1] |=> bm1
    _ -> throwError ev

scenario1 = [A 0, A 0, B 1, C 0]
scenario2 = [C 3, C 2, C 1, C 0, B 0]

basicSpec :: Spec
basicSpec = describe "basic" $ do
    let m = bm1
    it "arg-ok" $ isAccepted $ trans m [A 0]
    it "arg-ng" $ not $ isAccepted $ trans m [B 0]
    it "arg-recv-ok" $ isAccepted $ trans m [A 0 ,A 0]
    it "arg-recv-ng" $ not $ isAccepted $ trans m [A 0 ,A 1]
    it "arg-recv-ng" $ not $ isAccepted $ trans m [A 0 ,B 1]
    it "arg-send-ok" $ isAccepted $ trans m scenario1
    it "arg-send-ng" $ not $ isAccepted $ trans m [A 0 ,A 0, B 0]
    it "arg-send-ok'" $ isAccepted $ batchTrans m [A 0 ,A 0, C 0]
    it "continueTo-ok" $ isAccepted $ trans m [A 0 ,A 0, B 1, C 0, A 0, A 0, B 1]
    it "|=>-ok" $ isAccepted $ trans m scenario1
    it "|=>-ng" $ not $ isAccepted $ trans m $ scenario2 ++ [A 0]
    it "|=>-ok" $ isAccepted $ trans m [C 3 ,C 2, C 6]
    it "|=>-ok" $ isAccepted $ batchTrans m [C 3 ,B 0]

createMachine :: String -> Prism' Ev Int -> NamedMachine Ev M [Ev]
createMachine s prism = NamedMachine s $ \ev -> if
    | has prism ev -> do
        tell [ev]
        continueTo $ createMachine s prism
    | otherwise -> throwError ev

mA :: NamedMachine Ev M [Ev]
mA = createMachine "mA" _A

mB :: NamedMachine Ev M [Ev]
mB = createMachine "mB" _B

mC :: NamedMachine Ev M [Ev]
mC = createMachine "mC" _C

mAB :: NamedMachine Ev M [Ev]
mAB = NamedMachine "mAB" $ \ev -> case ev of
    A _ -> do
        tell [ev]
        continueTo mAB
    B _ -> do
        tell [ev]
        continueTo mAB
    _   -> throwError ev

mBC :: NamedMachine Ev M [Ev]
mBC = NamedMachine "mBC" $ \ev -> case ev of
    B _ -> do
        tell [ev]
        continueTo mBC
    C _ -> do
        tell [ev]
        continueTo mBC
    _ -> throwError ev

mABC :: NamedMachine Ev M [Ev]
mABC = NamedMachine "mABC" $ \ev -> case ev of
    A _ -> do
        tell [ev]
        continueTo mABC
    B _ -> do
        tell [ev]
        continueTo mABC
    C _ -> do
        tell [ev]
        continueTo mABC
    D _ -> do
        tell [ev]
        return []

mBC' :: NamedMachine Ev M [Ev]
mBC' = NamedMachine "mBC'" $ \ev -> case ev of
    B _ -> do
        tell [ev]
        args <- recv (^? _A)
        tell [A args]
        continueTo mBC'
    C _ -> do
        tell [ev]
        continueTo mBC'
    _   -> throwError ev


compSpec :: Spec
compSpec = describe "composition" $ do
    let m = bm1 >>> bm1
    it ">>>-ok" $ isAccepted $ trans m $ scenario1 ++ scenario1 ++ scenario2
    it ">>>-ng" $ not $ isAccepted $ trans m $ scenario1 ++ scenario2 ++ scenario2 ++ scenario1
    it ">>>-ng" $ not $ isAccepted $ trans m [D 0]
    let m = mAB |=| mBC
    it "|=|-ok" $ isAccepted $ trans m $ [A 0, A 0, B 0]
    it "|=|-ok" $ isAccepted $ trans m $ [B 0, A 0]
    it "|=|-ok" $ isAccepted $ trans m $ [B 0]
    it "|=|-ng" $ not $ isAccepted $ trans m $ [B 0, C 0]
    it "|=|-ng" $ not $ isAccepted $ trans m $ [A 0, C 0]
    it "|=|-ok" $ isAccepted $ trans m $ [C 0, C 0, B 0]
    it "|=|-ng" $ not $ isAccepted $ trans m $ [C 0, A 0]
    it "|=|-ng" $ not $ isAccepted $ trans m [D 0]
    let m = mA |=| mB |=| mC
    it "|=|-ok" $ isAccepted $ trans m $ [A 0, A 0]
    it "|=|-ok" $ isAccepted $ trans m $ [B 0, B 0]
    it "|=|-ok" $ isAccepted $ trans m $ [C 0, C 0]
    it "|=|-ng" $ not $ isAccepted $ trans m $ [A 0, B 0]
    it "|=|-ng" $ not $ isAccepted $ trans m $ [B 0, C 0]
    it "|=|-ng" $ not $ isAccepted $ trans m $ [C 0, A 0]
    let m = mAB \/ mBC
    it "\\/-ok" $ isAccepted $ trans m $ [A 0, A 0, B 0, C 0]
    it "\\/-ng" $ not $ isAccepted $ trans m $ [A 0, A 0, B 0, C 0, A 0]
    it "\\/-ng" $ not $ isAccepted $ trans m [D 0]
    let m = mA \/ mB \/ mC
    it "\\/-ok" $ isAccepted $ trans m $ [A 0, A 0]
    it "\\/-ok" $ isAccepted $ trans m $ [A 0, B 0, B 0]
    it "\\/-ok" $ isAccepted $ trans m $ [A 0, B 0, C 0, C 0]
    it "\\/-ok" $ isAccepted $ trans m $ [C 0, C 0]
    it "\\/-ng" $ not $ isAccepted $ trans m [B 0, A 0]
    it "\\/-ng" $ not $ isAccepted $ trans m [C 0, A 0]

parallelSpec :: Spec
parallelSpec = describe "parallel" $ do
    let m = interfaceP (has _B) mAB  mBC'
    it "interface-ok" $ isAccepted $ trans m [B 0, A 0] -- いつまでたっても mBC' 側は遷移出来ない
    it "interface-ok" $ isAccepted $ trans m [A 0]
    it "interface-ok" $ isAccepted $ trans m [C 0]
    it "interface-ng" $ not $ isAccepted $ trans m [B 0, B 0]
    let m = interfaceP (has _B)  mAB  $ interfaceP (has _B |.| has _C) mBC mABC
    --runIO $ print $ run $ trans m [A 0]
    it "interface-ok" $ 1 == (length $ run $ trans m [A 0])
    it "interface-ok" $ 3 == (length $ run $ trans m [B 0])
    it "interface-ok" $ 2 == (length $ run $ trans m [C 0])
    it "interface-finish-ng" $ not $ isAccepted $ trans m [D 0, D 0]
    it "interface-finish-ok" $ isAccepted $ trans m [D 0, A 0]
    it "interface-finish-ok" $ isAccepted $ trans m [D 0, C 0]
    it "interface-finish-ok" $ 3 == (length $ run $ trans m [D 0, B 0])
    --runIO $ print $ run $ trans m [D 0, B 0]
    let m = mAB  ||| mBC'
    it "|||-ok" $ isAccepted $ trans m [B 0]
    it "|||-ok" $ isAccepted $ trans m [A 0]
    it "|||-ok" $ isAccepted $ trans m [C 0]
    it "|||-ok" $ isAccepted $ trans m [B 0, B 0]
    it "|||-ng" $ not $ isAccepted $ trans m [D 0]
    let m = mA  ||| mB ||| mC
    it "|||-ok" $ isAccepted $ trans m [A 0, B 0, C 0, A 0, B 0, C 0]
    let m = (has _B, mAB) <||> (has _B |.| has _C, mBC')
    it "alphabet-ok" $ not $ isAccepted $ trans m [A 0]
    it "alphabet-ok" $ isAccepted $ trans m [B 0]
    it "alphabet-ok" $ isAccepted $ trans m [C 0]
    it "alphabet-ng" $ not $ isAccepted $ trans m [D 0]
    let m = alphabetP' [(has _B, mAB),  (has _B |.| has _C, mBC')]
    it "alphabet-ok" $ not $ isAccepted $ trans m [A 0]
    it "alphabet-ok" $ isAccepted $ trans m [B 0]
    it "alphabet-ok" $ isAccepted $ trans m [C 0]
    it "alphabet-ng" $ not $ isAccepted $ trans m [D 0]


test :: Int -> NamedMachine Ev M [Ev]
test n = NamedMachine ("test" ++ show n) $ \ev -> case ev of
    A _ -> continueTo $ test $ n + 1
    B _ -> continueTo $ test $ n + 1
    C _ -> continueTo $ test $ n + 1
    D _ -> continueTo $ (has _B |.| has _A, test $ n + 1) <||> (has _B |.| has _A, test $ 0)


traceCheck :: Spec
traceCheck = describe "composition" $ do

    let m = mABC >>> mA
    runIO $ print $ run $ trans m $ [C 0, D 1, A 0]
    
    let m = mA |=| mB
    runIO $ print $ run $ trans m $ [A 0, A 0]
    runIO $ print $ run $ trans m $ [A 0, B 0]
    runIO $ print $ run $ trans m $ [B 0, A 0]
    runIO $ print $ run $ trans m $ [B 0, B 0]

    let m = mA \/ mBC
    runIO $ print $ run $ trans m $ [A 0, A 0]
    runIO $ print $ run $ trans m $ [A 0, B 0]
    runIO $ print $ run $ trans m $ [B 0, A 0]
    runIO $ print $ run $ trans m $ [B 0, B 0]

    let m = (has _B |.| has _A |.| has _D, test 0) <||> (has _D |.| has _B |.| has _C, test 100)
    runIO $ print $ run $ trans m $ [A 0, A 0, B 0, C 0, A 0, B 0]
    runIO $ print $ run $ trans m $ [A 0, A 0, D 0, A 0, A 0]

main :: IO ()
main = do
    hspec $ describe "Test" $ do
        basicSpec
        compSpec
        parallelSpec
        traceCheck

