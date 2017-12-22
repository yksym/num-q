{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell, FlexibleContexts, MultiWayIf #-}
module Object
     ( module Object
     ) where

--import Data.Array
import Data.Set(insert)
import Data.List (nub, delete)
import Control.Lens
import Linear.V2 (V2(..), _x, _y)
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.State.Class
import Control.Monad.Reader
import Model
import Machine
import Text.Printf
import System.Random


-- memo  1:n サーバークライアント方式ゲーム記述
--
-- システム
--     リクエストに従ってモデルを更新し、コマンドをオブジェクトに通知する状態マシン
--     オブジェクトの生成はここの責務
--
-- オブジェクト
--     イベントに従ってシステムへのリクエストを出力する状態マシン
--     オブジェクト間では同期は行わず、システムとオブジェクトの間でのみ同期(ObjKeyを持っているイベント)
--
--これらを合成したものをゲームとする

type M        = ExceptT Event IO
type Base     = NamedMachine Event M [Event]
type Object s = NamedMachine Event (StateT s M) [Event]
type System   = Object World

type Game = (System, World)

runSystem :: World -> StateT World M a -> IO (Either Event (a, World))
runSystem w m = runExceptT $ runStateT m w

movingObj :: ObjKey -> Object MovingObj
movingObj key = NamedMachine ("movingObj@" ++ show key) $ \ev -> do
    isObjAlphabet key ev !& ev
    case ev of
        E_Cmd_Die _ -> do
            return []
        E_Cmd_Dir _ d -> do
            pDir .= d
            continueTo $ movingObj key
        _ -> throwError ev

movingObjTicker :: ObjKey -> Object MovingObj
movingObjTicker key = NamedMachine ("movingObjTicker@" ++ show key) $ \ev -> do
    isObjAlphabet key ev !& ev
    case ev of
        E_Cmd_Die _ -> do
            return []
        E_Cmd_Tick _ b fkey -> do
            fpos <- use pPos
            absv <- use pVel
            d <- use pDir
            let v = dir2vel absv d
            case findObject key b of
                Nothing -> return ()
                Just pos -> do
                    let Just pos = findObject key b
                    let fpos' = fpos + v
                    let pos' = floorV2 fpos'
                    when (pos /= pos') $ do
                        sendOr (E_Req_Move key pos pos') $ (movingObjTicker key) ^. machine
                        pDir .= DIR_N -- ここ外すとsnake
                    pPos .= fpos'
            continueTo $ movingObjTicker key
        _ -> throwError ev

movingObjInitializer :: ObjKey -> MovingObj -> System
movingObjInitializer key mo = liftNamedMachine $ unwrapST mo $
    interfaceP (== E_Cmd_Die key) (movingObjTicker key) $ movingObj key


initializer :: System
initializer = NamedMachine "initializer" $ \ev -> case ev of
    E_Ext_Tick -> do
        --n <- liftIO $ randomRs (0,10) <$> newStdGen
        let n = 2
        m <- liftIO $ fst <$> randomR (0,10) <$> newStdGen

        aa .= printf additionMonsterAA (showN n) (showN m) " ?"
        aa' .= printf additionMonsterAA' (showN n) (showN m) (showN $ n + m)

        let dummies = [k | k <- [1..20], abs (n + m - k) < 4]
        (e:ts) <- liftIO $ take 20 <$> nub <$> randomRs (V2 1 1, boardMaxIdx) <$> newStdGen
        b  <- use board
        b' <- liftIO $ foldM trap1 b $ zip dummies ts
        board .= b'
        --board . (ix (V2 1 1)) . (at wallKey) .= Just wall
        --board . (ix (V2 2 2)) . (at wallKey) .= Just wall

        [ E_Req_Add hero0 (" @", T_Hero) $ MovingObj (V2  0  0) DIR_N 0.3,
          E_Req_Add enemy0 (showN $ n + m, T_Enemy) $ MovingObj (fromIntegralV2 e) DIR_N 0.3
          ] |=> alphabetP'
            [ (isAnyObjAlphabet,  factory) -- ここから派生する全てのイベントをこいつがインターフェースとしてもつ必要がある
            , (has _E_Ext_Tick |.| has _E_Cmd_Tick, ticker)
            , (has _E_Req_Move |.| has _E_Cmd_Die, operator)
            , (has _E_Ext_Key |.| has _E_Cmd_Dir, subscriber)
            ]
    _ -> throwError ev

factory :: System
factory = NamedMachine "factory" $ \ev -> case ev of
    E_Req_Add key piece mo -> do
        fkey <- use freshKey
        fkey <= key !& ev
        let coord = floorV2 $ mo ^. pPos
        c <- use $ board . (ix coord)
        let c' = omInsert key piece c
        (keys, c'') <- updateCell c' ?& ev
        length keys == 0 !& ev
        board . (ix  coord) .= c''
        freshKey .= succ key
        objKeys %= insert key
        continueTo $ (isAnyObjAlphabet, factory) <||> (isObjAlphabet key, movingObjInitializer key mo)
    _ -> continueTo factory

ticker :: System
ticker = NamedMachine "ticker" $ \ev -> case ev of
    E_Ext_Tick -> do
        mode' <- use $ mode
        when (_Running `has` mode') $ do
            keys <- use objKeys
            forM_ keys $ \key -> do
                b <- use $ board
                fkey <- use freshKey
                send $ E_Cmd_Tick key b fkey
        w <- get
        continueTo ticker
    _ -> throwError ev


operator :: System
operator = NamedMachine "operator" $ \ev -> case ev of
    E_Req_Move key from to -> do
        b <- use $ board
        cf <- b ^? (ix from) ?& ev
        ct <- b ^? (ix to) ?& ev
        piece <- cf ^. (at key) ?& ev
        let ct' = omInsert key piece ct
        (keys, ct'') <- updateCell ct' ?& ev
        forM_ keys $ \k -> do
            when (ct' ^? (at k) . _Just . _2 == Just T_Hero) $ do
                mode .= GameOver
                tmp <- use aa'
                aa .= tmp
            when (ct' ^? (at k) . _Just . _2 == Just T_Enemy) $ do
                mode .= GameClear
                aa .= downAA
            send $ E_Cmd_Die k
        board . (ix from) %= omDelete [key]
        board . (ix to) .= ct''
        continueTo operator
    _ ->  throwError ev

subscriber :: System
subscriber = NamedMachine "subscriber" $ \ev -> case ev of
    E_Ext_Key s -> do
        case s of
            "Up" -> send $ E_Cmd_Dir hero0 DIR_U
            "Down" -> send $ E_Cmd_Dir hero0 DIR_D
            "Right" -> send $ E_Cmd_Dir hero0 DIR_R
            "Left" -> send $ E_Cmd_Dir hero0 DIR_L
            "K" -> send $ E_Cmd_Dir enemy0 DIR_U
            "J" -> send $ E_Cmd_Dir enemy0 DIR_D
            "L" -> send $ E_Cmd_Dir enemy0 DIR_R
            "H" -> send $ E_Cmd_Dir enemy0 DIR_L
            _      -> throwError ev
        continueTo subscriber
    _ -> throwError ev

perform :: Game -> Event -> IO Game
perform g@(sys, w) ev = do
    result <- runSystem w $ batchTransSkipError sys [ev]
    case result of
        Left _ -> do
            error $ "fail: " ++ show ev
            return g
        Right (Just sys', w') -> do
            return (sys', w')

initGame :: Game
initGame = (initializer, initWorld)



test :: IO ()
test = do
    let g = initGame
    g <- perform g E_Ext_Tick
    g <- perform g E_Ext_Tick
    g <- perform g $ E_Ext_Key "J"
    g <- perform g $ E_Ext_Key "Left"
    g <- perform g E_Ext_Tick
    g <- perform g E_Ext_Tick
    return ()


