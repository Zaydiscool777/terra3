{-# LANGUAGE LexicalNegation, TupleSections, TemplateHaskell #-}
module Lib ( tilClose, init', tick ) where

import Raylib.Core as R
-- import Raylib.Core.Audio as Audio
-- import Raylib.Core.Models as Model
-- import Raylib.Core.Shapes as Shape
import Raylib.Core.Text as Text
import Raylib.Core.Textures as Texture

import Raylib.Types.Core
-- import Raylib.Types.Core.Audio
import Raylib.Types.Core.Camera
-- import Raylib.Types.Core.Models as Models
-- import Raylib.Types.Core.Text as Text
import Raylib.Types.Core.Textures as Textures

import Raylib.Util.Colors
import Raylib.Util.Lenses

import Linear
import Control.Monad
import Control.Monad.State
import Control.Lens
import Data.Maybe
import qualified Data.Map as M
import Control.Applicative (Alternative(empty))

-- types

data Player = Player {
  _pos :: Vector2,
  _uvel :: Vector2,
  _tvel :: Vector2,
  _air :: Bool
} deriving Show
makeLenses ''Player

data Model = Model {
  _player :: Player,
  _image :: (Texture, M.Map Char Texture),
  _lvlnum :: Int
}
makeLenses ''Model

data Action =
  AStart
  | AEnd
  | AKey KeyboardKey
  deriving Show


-- elm

init' :: IO Model
init' = do
  img1i <- loadImage "./ball.png"
  img1r <- imageResize img1i (playerRad * 2) (playerRad * 2)
  img1 <- loadTextureFromImage img1r
  img2i <- loadImage "./wall.png"
  img2r <- imageResize img2i tilesLen tilesLen
  img2 <- loadTextureFromImage img2r
  return Model {
    _player = Player {
      _pos = pure 200,
      _uvel = pure 0,
      _tvel = pure 0,
      _air = True
    },
    _image = (img1, M.fromList [('1', img2)]),
    _lvlnum = 0
  }

tick :: StateT Model IO ()
tick = do
  model <- get
  new <- liftIO (sub model)
  liftIO (view' model)
  put new

sub :: Model -> IO Model
sub i = do
  let i1 = upd i AStart
  x <- getKeysDown
  let i2 = foldl' upd i1 (AKey <$> x)
  return $ upd i2 AEnd

upd :: Model -> Action -> Model
upd m AStart = m & player.uvel .~ pure 0
upd m (AKey KeyUp) = m & player.air .~ True & player.uvel._y %~ if m^.player.air then id else const (-22)
upd m (AKey KeyDown) = m & player.uvel._y %~ (+ 10)
upd m (AKey KeyLeft) = m & player.uvel._x %~ (- 10)
upd m (AKey KeyRight) = m & player.uvel._x %~ (+ 10)
upd m (AKey KeyR) = m & player .~
  Player {
    _pos = pure 201,
    _uvel = pure 0,
    _tvel = pure 0,
    _air = True
  }
upd m AEnd = inputted m
upd m _ = m

view' :: Model -> IO ()
view' m = do
  beginDrawing
  clearBackground rayWhite
  w <- getScreenWidth
  h <- getScreenHeight
  beginMode2D (center (m^.player.pos) w h)
  drawtiles (tilesr (m^.lvlnum)) (m^.image._2)
  drawplayer (m^.player.pos) (m^.image._1)
  endMode2D
  endDrawing -- presents frame to user
  drawText (unlines $ chunksOf 25 $ show $ m^.player) 0 100 24 darkGray

-- io

drawtiles :: [(Char, Rectangle)] -> M.Map Char Texture -> IO ()
drawtiles t i = mapM_ f t -- insert monadic fold over map
  where
    f (id', x) =
      case M.lookup id' i of
        Just i' -> drawTextureV i' (V2 (x^._rectangle'x) (x^._rectangle'y)) red
        Nothing -> return ()

drawplayer :: Vector2 -> Texture -> IO ()
drawplayer v i = drawTextureV i (v - pure playerRad) white

-- pure

center :: Vector2 -> Int -> Int -> Camera2D
center v c d = Camera2D ((/ 2) . fromIntegral <$> V2 c d) v 0 1

move :: Int -> Player -> Player
move l m@Player {_pos = p, _uvel = v, _tvel = t, _air = a} =
  m {_pos = p' + t', _tvel = t', _air = a'}
  where
    t1 = (v + V2 0 1 +) $ t & _x %~ (* 0.7)
    (p', t', a') = foldr block (p, t1, a) [y | (x, y) <- tilesr l, x == '1']

block :: Rectangle -> (Vector2, Vector2, Bool) -> (Vector2, Vector2, Bool)
block (Rectangle rx ry rw rh) t@(V2 px py, V2 tx ty, _) =
  (if inside_h -- down (w/ floor)
  && py + pr <= ry && py + ty + pr >= ry
  then (_1._y .~ ry - pr) . (_2._y .~ 0) . (_3 .~ False) else id) $ 
  (if inside_h -- up
  && py - pr >= ry + rh && py + ty - pr <= ry + rh
  then (_1._y .~ ry + rh + pr) . (_2._y .~ 0) else id) $
  (if inside_v -- right
  && px + pr <= rx && px + tx + pr >= rx
  then (_1._x .~ rx - pr) . (_2._x .~ 0) else id) $
  (if inside_v -- left
  && px - pr >= rx + rw && px + tx - pr <= rx + rw
  then (_1._x .~ rx + rw + pr) . (_2._x .~ 0) else id)
  t
  where
    pr = playerRad
    inside_h = px + pr > rx && px - pr < rx + rw
    inside_v = py + pr > ry && py - pr < ry + rh

inputted :: Model -> Model
inputted m = m & player %~ move (m^.lvlnum) 

-- utils

-- untilM :: Monad m => (a -> Bool) -> m a -> m a from: utility-ht
loopUntil :: Monad m => (a -> Bool) -> StateT s m a -> s -> m [a]
loopUntil p m s = do
  (x, s') <- runStateT m s
  if p x then return [] else (x :) <$> loopUntil p m s'

-- from: split
chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (splitter ls (:) [])
 where
  splitter :: [e] -> ([e] -> a -> a) -> a -> a
  splitter [] _ n = n
  splitter l c n = l `c` splitter (drop i l) c n

-- also :: (a -> Bool) -> (a -> State s a) -> a -> State s [a]
-- untilM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m [a]
-- untilM p m = loopUntil p (get >>= lift . m)

untilM' :: Monad m => (a -> Bool) -> m a -> m [a]
untilM' p m = loopUntil p (lift m) ()

-- lesser utils

tilClose :: StateT Model IO () -> StateT Model IO ()
tilClose x = void $ untilM' id (x >> liftIO windowShouldClose)

-- getKeysPressed :: IO [KeyboardKey]
-- getKeysPressed = untilM' (== KeyNull) getKeyPressed

keysint :: [Int]
keysint = join [[0,4,5,24,25,32,39,59,61,96],[44..57],[65..93],[256..269],[280..284],[290..301],[320..336],[340..348]]

getKeysDown :: IO [KeyboardKey]
getKeysDown = filterM isKeyDown (toEnum <$> keysint)

-- data 

tilesr :: Int -> [(Char, Rectangle)]
tilesr i = (\(id', row, col) -> (id', Rectangle (col * k) (row * k) k k)) <$> tiles !! i
  where k = tilesLen

tiles :: [[(Char, Float, Float)]]
tiles = map makeTiles
  [lines
    "       1\n\
    \1 11111111111\n\
    \1\n\
    \111111   11111\n\
    \ \n\
    \   111111111",
  []]

makeTiles :: [String] -> [(Char, Float, Float)]
makeTiles = join . zipWith (\i r -> catMaybes $ zipWith (\j x -> Just (x, i, j)) [0..] r) [0..]

playerRad, tilesLen :: Num a => a
playerRad = 15
tilesLen = 100
