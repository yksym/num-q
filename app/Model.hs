{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell, FlexibleContexts, MultiWayIf #-}
module Model
     ( module Model
     ) where

import Data.Array
import qualified Data.IntMap as IM
import Control.Monad
import Data.List (intercalate)
import Data.Set (Set, empty, member, insert, fromList, toList)
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
  | T_Trap
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

type Board = Array Coord (ObjMap Piece)

data GameMode
  = Running
  | GameClear
  | GameOver
  deriving (Eq, Show)

data MoveMode
  = M_Slide
  | M_Step
  deriving (Eq, Show)

makePrisms ''GameMode

data World = World
  { _board  :: Board
  , _mode   :: GameMode
  , _msg    :: String
  , _cnt    :: Int
  , _aa     :: String
  , _aa'    :: String
  , _objKeys  :: Set ObjKey
  --, _tickTargets  :: Set ObjKey -- デフォルトはobjKeys
  } deriving (Eq, Show)

makeLenses ''World

data MovingObj = MovingObj
  { _pPos    :: FCoord
  , _pDir    :: Direction
  , _pSpeed  :: Float
  , _pMoveMode :: MoveMode
  , _pVel    :: FCoord
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
  | E_Cmd_Tick ObjKey Board
  | E_Cmd_Dir  ObjKey Direction
  | E_Cmd_Attack ObjKey
  | E_Cmd_Die  ObjKey
  deriving (Eq)

makePrisms ''Event

instance Show Event where
    show E_Ext_Tick = "E_Ext_Tick"
    show (E_Ext_Key s) = "E_Ext_Key " ++ s
    show (E_Req_Add k _ _) = "E_Req_Add " ++ show k
    show (E_Req_Move k f t) = "E_Req_Move " ++ show k ++ " " ++ show (f, t)
    show (E_Cmd_Tick k _) = "E_Cmd_Tick " ++ show k
    show (E_Cmd_Dir k d) = "E_Cmd_Dir " ++ show k ++ " " ++ show d
    show (E_Cmd_Attack k) = "E_Cmd_Attack " ++ show k
    show (E_Cmd_Die k) = "E_Cmd_Die " ++ show k


isObjAlphabet :: ObjKey -> Event -> Bool
isObjAlphabet key (E_Req_Move key' _ _  ) = key == key'
isObjAlphabet key (E_Req_Add  key' _ _) = key == key'
isObjAlphabet key (E_Cmd_Tick key' _ ) = key == key'
isObjAlphabet key (E_Cmd_Dir  key' _    ) = key == key'
isObjAlphabet key (E_Cmd_Attack key'    ) = key == key'
isObjAlphabet key (E_Cmd_Die  key'      ) = key == key'
isObjAlphabet _ _ = False

isAnyObjAlphabet :: Event -> Bool
isAnyObjAlphabet (E_Req_Move _ _ _ ) = True
isAnyObjAlphabet (E_Req_Add  _ _ _ ) = True
isAnyObjAlphabet (E_Cmd_Tick _ _ ) = True
isAnyObjAlphabet (E_Cmd_Dir  _ _   ) = True
isAnyObjAlphabet (E_Cmd_Attack  _     ) = True
isAnyObjAlphabet (E_Cmd_Die  _     ) = True
isAnyObjAlphabet _ = False

showN :: Int -> String
showN x = printf "%2d" x

boardMaxIdx :: (Num a) => V2 a
boardMaxIdx = V2 (boardW-1) (boardH -1)
    where
        boardW = 32
        boardH = 32

boardSize :: Board -> (Int, Int)
boardSize b = let (_, V2 w1 h1) = bounds b in (w1 + 1, h1 + 1)


showBoard :: Board -> String
showBoard b = intercalate "\n" [cellsInRow r | r <- [0..(h-1)]]
  where
    (w, h) = boardSize b
    cellsInRow :: Int -> String
    cellsInRow y = concat [showCell $ b ! (V2 x y) | x <- [0..(w-1)]]
    showCell c = case omElems c of
        ((sym, _) : _) -> sym
        _ -> "  "

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
        (ix c) . (at trapKey) .= (Just $ (showN $ n, T_Trap))

findObject :: ObjKey -> Board -> Maybe Coord
findObject k b = fst <$> ifind pred b
    where
    pred :: Coord -> ObjMap Piece -> Bool
    pred i objs = k `omMember` objs

hero0 :: ObjKey
hero0 = 100

enemy0 :: ObjKey
enemy0 = 101

fire :: ObjKey
fire = 102

wallKey :: ObjKey
wallKey =  1

wall :: Piece
wall = (" X", T_Wall)

trapKey :: ObjKey
trapKey =  1

initBoard :: Board
initBoard = (listArray (V2 0 0, boardMaxIdx) $ repeat omEmpty) &~ do
    forM_ [0..boardMaxIdx ^. _y] $ \y ->
        forM_ [0..boardMaxIdx ^. _x] $ \x ->
            when (x == 0 || y == 0 || x == boardMaxIdx ^. _x || y == boardMaxIdx ^. _y) $ (ix (V2 x y)) . (at wallKey) .= Just wall

initWorld :: World
initWorld = World
    { _board = initBoard
    , _mode = Running
    , _msg  = explanation
    , _cnt  =  30 * 30 -- 30 sec @ 30 fps
    , _aa   =  ""
    , _aa'  =  ""
    , _objKeys  = empty
    }

findObjKeysByType :: ObjType -> Cell -> [ObjKey]
findObjKeysByType ot c = [k | (k, (_, ot')) <- omAssocs c, ot == ot' ]

hasAllObjTypes :: [ObjType] -> Cell -> Bool
hasAllObjTypes ots c = all (not . null) [findObjKeysByType ot c | ot <- ots]

updateCell :: Cell -> Maybe ([ObjKey], Cell)
updateCell c = do
    guard $ not $ hasAllObjTypes [T_Wall, T_Hero] c
    guard $ not $ hasAllObjTypes [T_Wall, T_Enemy] c
    guard $ 1 >= (length $ findObjKeysByType T_Hero c)
    guard $ 1 >= (length $ findObjKeysByType T_Enemy c)
    if
        | hasAllObjTypes [T_Attack, T_Wall] c -> do
            let keys = findObjKeysByType T_Attack c
            return $ (keys, omDelete keys c)
        | hasAllObjTypes [T_Attack, T_Enemy] c -> do
            let keysA = findObjKeysByType T_Attack c
            let keysE = findObjKeysByType T_Enemy c
            let keys = keysA ++ keysE
            return $ (keys, omDelete keys c)
        | hasAllObjTypes [T_Trap] c -> do
            let keysH  = findObjKeysByType T_Hero c
            let keysE = findObjKeysByType T_Enemy c
            let keysA = findObjKeysByType T_Attack c
            let keysT = findObjKeysByType T_Trap c
            let keys = if (not $ null $ keysA)
                then keysA ++ keysT ++ keysH ++ keysE
                else keysH ++ keysE
            return $ (keys, omDelete keys c)
        | hasAllObjTypes [T_Hero, T_Enemy] c -> do
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

explanation :: String
explanation = "\
    \                                    \n\
    \ Guess the answer, then shoot it!!  \n\
    \                                    \n\
    \ right arrow : Move                 \n\
    \ left  arrow : Move                 \n\
    \ up    arrow : Move                 \n\
    \ down  arrow : Move                 \n\
    \                                    \n\
    \ Enter       : Attack               \n\
    \                                    \n\
    \ r           : Restart              \n\
    \ q           : Quit                 \n\
    \                                    \n\
    \                                     "


