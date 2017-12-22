{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell, FlexibleContexts, MultiWayIf #-}
module Model
     ( module Model
     -- , module Linear.V2
     ) where

import Data.Array
import qualified Data.IntMap as IM
import Control.Monad
import Data.Set(Set, empty, member, insert, fromList, toList)
import Control.Lens
import Linear.V2 (V2(..), _x, _y)
import Machine
import Text.Printf
import System.Random

instance (Random a) => Random (V2 a) where
  randomR (V2 lx ly, V2 ux uy) g = (V2 x y, g'')
    where
        (x,g') = randomR (lx, ux) g
        (y,g'') = randomR (lx, ux) g'
  random g = (V2 x y, g'')
    where
        (x,g') = random g
        (y,g'') = random g'

type ObjKey = Int

type Coord = V2 Int
type FCoord = V2 Float

type Symbol = String
type AA = String

data ObjType
  = T_Hero
  | T_Enemy
  | T_Attack
  | T_Wall
  deriving (Eq, Ord, Show)

type Piece = (Symbol, ObjType)

type ObjMap = IM.IntMap -- ObjKey をキーとする

omAssocs :: ObjMap a -> [(ObjKey, a)]
omAssocs = IM.assocs

omElems :: ObjMap a -> [a]
omElems = IM.elems

omKeys :: ObjMap a -> [ObjKey]
omKeys = IM.keys

omMember :: ObjKey -> ObjMap a -> Bool
omMember = IM.member

omInsert :: ObjKey -> a -> ObjMap a -> ObjMap a
omInsert = IM.insert

omDelete :: [ObjKey] -> ObjMap a -> ObjMap a
omDelete ks om = foldr IM.delete om ks

omEmpty :: ObjMap a
omEmpty = IM.empty

type Cell = ObjMap Piece
data Direction
  = DIR_U
  | DIR_D
  | DIR_R
  | DIR_L
  | DIR_N
  deriving (Eq, Show)

isOppositeDir :: Direction -> Direction -> Bool
isOppositeDir DIR_U DIR_D = True
isOppositeDir DIR_D DIR_U = True
isOppositeDir DIR_R DIR_L = True
isOppositeDir DIR_L DIR_R = True
isOppositeDir _ _ = False

dir2vel :: Float -> Direction -> FCoord
dir2vel v DIR_U = V2 0 (-v)
dir2vel v DIR_D = V2 0 v
dir2vel v DIR_R = V2 v 0
dir2vel v DIR_L = V2 (-v) 0
dir2vel v DIR_N = V2 0 0

type Board = Array Coord (ObjMap Piece)

data Mode
  = Running
  | GameClear
  | GameOver
  deriving (Eq, Show)

makePrisms ''Mode

data World = World
  { _board  :: Board
  , _mode   :: Mode
  , _msg    :: String
  , _aa     :: String
  , _aa'    :: String
  , _freshKey :: ObjKey
  , _objKeys  :: Set ObjKey
  --, _tickTargets  :: Set ObjKey -- デフォルトはobjKeys
  } deriving (Eq, Show)

makeLenses ''World

data MovingObj = MovingObj
  { _pPos    :: FCoord
  , _pDir    :: Direction
  , _pVel    :: Float
  } deriving (Eq, Show)

makeLenses ''MovingObj

data Event
  -- 外部イベント
  = E_Ext_Tick
  | E_Ext_Key  String
  -- オブジェクトからの要求
  | E_Req_Add ObjKey Piece MovingObj
  | E_Req_Move ObjKey Coord Coord
  -- オブジェクトへの通知
  | E_Cmd_Tick ObjKey Board ObjKey
  | E_Cmd_Dir  ObjKey Direction
  | E_Cmd_Die  ObjKey
  deriving (Eq)

makePrisms ''Event

instance Show Event where
    show E_Ext_Tick = "E_Ext_Tick"
    show (E_Ext_Key s) = "E_Ext_Key " ++ s
    show (E_Req_Add k _ _) = "E_Req_Add " ++ show k
    show (E_Req_Move k f t) = "E_Req_Move " ++ show k ++ " " ++ show (f, t)
    show (E_Cmd_Tick k _ _) = "E_Cmd_Tick " ++ show k
    show (E_Cmd_Dir k d) = "E_Cmd_Dir " ++ show k ++ " " ++ show d
    show (E_Cmd_Die k) = "E_Cmd_Die " ++ show k


isObjAlphabet :: ObjKey -> Event -> Bool
isObjAlphabet key (E_Req_Move key' _ _  ) = key == key'
isObjAlphabet key (E_Req_Add  key' _ _) = key == key'
isObjAlphabet key (E_Cmd_Tick key' _  _ ) = key == key'
isObjAlphabet key (E_Cmd_Dir  key' _    ) = key == key'
isObjAlphabet key (E_Cmd_Die  key'      ) = key == key'
isObjAlphabet _ _ = False

isAnyObjAlphabet :: Event -> Bool
isAnyObjAlphabet (E_Req_Move _ _ _ ) = True
isAnyObjAlphabet (E_Req_Add  _ _ _ ) = True
isAnyObjAlphabet (E_Cmd_Tick _ _ _ ) = True
isAnyObjAlphabet (E_Cmd_Dir  _ _   ) = True
isAnyObjAlphabet (E_Cmd_Die  _     ) = True
isAnyObjAlphabet _ = False

showN :: Int -> String
showN x = printf "%2d" x

boardMaxIdx :: (Num a) => V2 a
boardMaxIdx = V2 (boardW-1) (boardH -1)
    where
        boardW = 30
        boardH = 30

boardSize :: Board -> (Int, Int)
boardSize b = let (_, V2 w1 h1) = bounds b in (w1 + 1, h1 + 1)

clamp :: (Ord a) => (a, a) -> a -> a
clamp (l, u) = max l . min u

clampV2 :: (Ord a) => (V2 a, V2 a) -> V2 a -> V2 a
clampV2 (V2 xl yl, V2 xu yu) (V2 x y) = V2 (clamp (xl, xu) x) (clamp (yl, yu) y)

clampPos :: FCoord -> FCoord
clampPos pos = clampV2 (V2 0 0, boardMaxIdx) $ pos

floorV2 :: FCoord -> Coord
floorV2 (V2 x y) = V2 (floor x) (floor y)

fromIntegralV2 :: Coord -> FCoord
fromIntegralV2 (V2 x y) = V2 (fromIntegral x) (fromIntegral y)

trap1 :: Board -> (Int, Coord) -> IO Board
trap1 b (n,c) = do
    return $ b &~ do
        (ix c) . (at trapKey) .= (Just $ (showN $ n, T_Attack))

findObject :: ObjKey -> Board -> Maybe Coord
findObject k b = fst <$> ifind pred b
    where
    pred :: Coord -> ObjMap Piece -> Bool
    pred i objs = k `omMember` objs

hero0 :: ObjKey
hero0 = 100

enemy0 :: ObjKey
enemy0 = 101

wallKey :: ObjKey
wallKey =  1

wall :: Piece
wall = (" X", T_Wall)

trapKey :: ObjKey
trapKey =  1

initBoard :: Board
initBoard = listArray (V2 0 0, boardMaxIdx) $ repeat omEmpty
-- b &~ do ...

initWorld :: World
initWorld = World
    { _board = initBoard
    , _mode = Running
    , _msg  = ""
    , _aa   =  ""
    , _aa'  =  ""
    , _freshKey = 10
    , _objKeys  = empty
    }

findObjKeysByType :: ObjType -> Cell -> [ObjKey]
findObjKeysByType ot c = [k | (k, (_, ot')) <- omAssocs c, ot == ot' ]

hasObjTypes :: [ObjType] -> Cell -> Bool
hasObjTypes ots c = all id [length (findObjKeysByType ot c) > 0 | ot <- ots]

updateCell :: Cell -> Maybe ([ObjKey], Cell)
updateCell c = do
    guard $ not $ hasObjTypes [T_Wall, T_Hero] c
    guard $ not $ hasObjTypes [T_Wall, T_Enemy] c
    guard $ 1 >= (length $ findObjKeysByType T_Hero c)
    guard $ 1 >= (length $ findObjKeysByType T_Enemy c)
    if
        | hasObjTypes [T_Attack] c -> do
            let keys  = findObjKeysByType T_Hero c
            let keys' = keys ++ findObjKeysByType T_Enemy c
            return $ (keys', omDelete keys' c)
        | hasObjTypes [T_Hero, T_Enemy] c -> do
            let keys = findObjKeysByType T_Enemy c
            return $ (keys, omDelete keys c)
        | otherwise -> return ([], c)


downAA :: AA
downAA = "\
            \                       \n\
            \                       \n\
            \                       \n\
            \                       \n\
            \                       \n\
            \   _(:3 J /_)_         \n\
            \                       \n\
            \                         "


additionMonsterAA :: AA
additionMonsterAA = "\
            \                                   \n\
            \                                   \n\
            \            . o O ( %s + %s = %s ) \n\
            \      ^  ^                         \n\
            \  　( ` w ')                       \n\
            \  　/   +  O---->                  \n\
            \  　  u -J                         \n\
            \                                   \n\
            \  Addition Monster                 \n\
            \                                   "

additionMonsterAA' :: AA
additionMonsterAA' = "\
            \                                     \n\
            \                                     \n\
            \            ^ . o O ( %s + %s = %s ) \n\
            \      ^  ^  |                        \n\
            \  　( - w -)|                        \n\
            \  　/   +   O                        \n\
            \  　  u -J  |                        \n\
            \                                     \n\
            \  Addition Monster                   \n\
            \                                   "
