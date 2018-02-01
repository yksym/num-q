{-# LANGUAGE OverloadedStrings, MultiWayIf #-}
module Main
  ( main
  ) where

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Monoid ((<>))
import qualified Brick.Main as M
import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , fill
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>)
  , (<=>)
  , viewport
  , ViewportType(Horizontal, Vertical, Both)
  , strWrapWith
  )
import Text.Wrap (defaultWrapSettings, preserveIndentation)
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Linear.V2 (V2(..))
import Lens.Micro ((^.))
import Data.Array
import Object -- Machine
import Model

data Tick = Tick

app :: App Game Tick Name
app = App { appDraw = drawUI . snd
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay $ 33 * 1000
  void $ customMain (V.mkVty V.defaultConfig) (Just chan) app initGame


-- Handling events

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)                       = continue =<< (liftIO $ perform g E_Ext_Tick)
handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue =<< (liftIO $ perform g $ E_Ext_Key "Up" )
handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue =<< (liftIO $ perform g $ E_Ext_Key "Down" )
handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue =<< (liftIO $ perform g $ E_Ext_Key "Right" )
handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue =<< (liftIO $ perform g $ E_Ext_Key "Left" )
handleEvent g (VtyEvent (V.EvKey V.KEnter []))      = continue =<< (liftIO $ perform g $ E_Ext_Key "Enter" )
handleEvent g (VtyEvent (V.EvKey (V.KChar 'k') [])) = continue =<< (liftIO $ perform g $ E_Ext_Key "K" )
handleEvent g (VtyEvent (V.EvKey (V.KChar 'j') [])) = continue =<< (liftIO $ perform g $ E_Ext_Key "J" )
handleEvent g (VtyEvent (V.EvKey (V.KChar 'l') [])) = continue =<< (liftIO $ perform g $ E_Ext_Key "L" )
handleEvent g (VtyEvent (V.EvKey (V.KChar 'h') [])) = continue =<< (liftIO $ perform g $ E_Ext_Key "H" )
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = continue initGame
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g _                                     = continue g

data Name = VP deriving (Ord, Show, Eq)

drawUI :: World -> [Widget Name]
drawUI g = [ padLeft (Pad 5) $ padTop (Pad 5) $ (padRight (Pad 5) $ drawGrid g) <+> (drawAA g <=> (padTop (Pad 2) $ drawMsg g))]

drawMsg :: World -> Widget Name
drawMsg g = hLimit 40 $ vLimit 20 $ strWrapWith (defaultWrapSettings { preserveIndentation = True }) $ s
  where
    s = if
         | g ^. mode == GameOver -> "GAME OVER"
         | g ^. mode == GameClear -> "GAME CLEAR"
         | otherwise -> g ^. msg


drawAA :: World -> Widget Name
drawAA g = hLimit 40 $ vLimit 20 $ str $ g ^. aa

drawGrid :: World -> Widget Name
drawGrid g = -- withBorderStyle BS.ascii
  -- $ B.border $
  vBox rows
  where
    (w, h) = boardSize $ g ^. board
    rows         = [hBox $ cellsInRow r | r <- [0..(h-1)]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..(w-1)]]
    drawCoord    = drawCell . cellAt
    cellAt c = (g ^. board) ! c

drawCell :: Cell -> Widget Name
drawCell c = case omElems c of
    ((sym, _) : _) -> withAttr defaultAttr $ str sym
    [] -> withAttr defaultAttr $ str "  "

theMap :: AttrMap
theMap = attrMap V.defAttr []

defaultAttr :: AttrName
defaultAttr = "default"
