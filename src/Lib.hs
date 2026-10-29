{-# LANGUAGE BlockArguments, LexicalNegation, TemplateHaskell, TupleSections #-}
module Lib where

import Raylib.Core as R
-- import Raylib.Core.Audio as Audio
-- import Raylib.Core.Models as Model
import Raylib.Core.Shapes as Shape
import Raylib.Core.Text as Text
-- import Raylib.Core.Textures as Texture

import Raylib.Types.Core
-- import Raylib.Types.Core.Audio
import Raylib.Types.Core.Camera
-- import Raylib.Types.Core.Models as Models
-- import Raylib.Types.Core.Text as Text
-- import Raylib.Types.Core.Textures as Textures

import Raylib.Util.Colors

import Linear
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Control.Lens
import Data.Bool
import Debug.Trace

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

test = block a b
  where
    (a, b, _) =
      (Rectangle {rectangle'x = 0.0, rectangle'y = 300.0, rectangle'width = 100.0, rectangle'height = 100.0},
      (V2 116.770004 300.0, V2 (-10.0) 0.0, False),
      (V2 100.0 300.0, V2 0.0 0.0, False))
    block r@(Rectangle rx ry rw rh) t@(V2 px py, V2 tx ty, _) =
      (if inside_v -- left
      && (px - pr >= rx + rw) && (px + tx - pr <= rx + rw)
      then (_1._x .~ rx + rw) . (_2._x .~ 0) else id)
      t
      where
        pr = 10
        inside_h = (px + pr >= rx) && (px - pr <= rx + rw)
        inside_v = (py + pr >= ry) && (py - pr <= ry + rh)

tilClose :: StateT Model IO () -> StateT Model IO ()
tilClose x = void $ untilM' id (x >> liftIO windowShouldClose)

-- getKeysPressed :: IO [KeyboardKey]
-- getKeysPressed = untilM' (== KeyNull) getKeyPressed

keysint :: [Int]
keysint = join [[0,4,5,24,25,32,39,59,61,96],[44..57],[65..93],[256..269],[280..284],[290..301],[320..336],[340..348]]

getKeysDown :: IO [KeyboardKey]
getKeysDown = filterM isKeyDown (toEnum <$> keysint)

-- elm

data Model = Model {
  _player :: Vector2,
  _uvel :: Vector2,
  _tvel :: Vector2,
  _air :: Bool
} deriving Show
makeLenses ''Model

init' :: Model
init' = Model {
  _player = pure 201,
  _uvel = pure 0,
  _tvel = pure 0,
  _air = True
}

tick :: StateT Model IO ()
tick = do
  model <- get
  new <- liftIO (sub model)
  put new
  liftIO (view' model)

data Action =
  AStart
  | AEnd
  | AKey KeyboardKey
  deriving Show

sub :: Model -> IO Model
sub i = do
  i1 <- upd i AStart
  x <- getKeysDown
  i2 <- foldM upd i1 (AKey <$> x)
  upd i2 AEnd

upd :: Model -> Action -> IO Model
upd m AStart = return m {_uvel = pure 0}
upd m (AKey KeyUp) = return $ m & air .~ True & uvel._y %~ if _air m then id else const (-22)
upd m (AKey KeyDown) = return $ m & uvel._y %~ (+ 10)
upd m (AKey KeyLeft) = return $ m & uvel._x %~ (- 10)
upd m (AKey KeyRight) = return $ m & uvel._x %~ (+ 10)
upd _ (AKey KeyR) = return init'
upd m AEnd = return (move m)
upd m _ = return m

view' :: Model -> IO ()
view' m = do
  beginDrawing
  clearBackground rayWhite
  w <- getScreenWidth
  h <- getScreenHeight
  beginMode2D (center (_player m) w h)
  drawtiles
  drawCircleV (_player m) playerRad red 
  drawCircleV (_player m + _tvel m) playerRad (blue {color'a = 50})
  endMode2D
  endDrawing -- presents frame to user
  drawText (unlines $ chunksOf 25 $ show m) 0 100 24 darkGray
  waitTime 0.03

-- io

drawtiles :: IO ()
drawtiles = mapM_ (`drawRectangleRec` black) tilesr

drawplayer :: Vector2 -> IO ()
drawplayer v = drawCircleV v playerRad red

-- pure

tiles :: [[Integer]]
tiles = fmap (fmap (bool 0 1 . (== '1'))) [
  "0000001",
  "011111111111",
  "",
  "1111100011111"]

tilesr' :: Float -> [Rectangle]
tilesr' k = (\(row, col) -> Rectangle (col * k) (row * k) k k) <$> (m >>= catMaybes)
  where m = zipWith (\i r -> zipWith (\j x -> guard (x == 1) >> Just (i, j)) [0..] r) [0..] tiles

playerRad :: Float
playerRad = 10

tilesr :: [Rectangle]
tilesr = tilesr' 100 -- [Rectangle 100 100 400 125]

center :: Vector2 -> Int -> Int -> Camera2D
center v c d = Camera2D ((/ 2) . fromIntegral <$> V2 c d) v 0 1

move :: Model -> Model
move m@Model {_player = p, _uvel = v, _tvel = t, _air = a} =
  m {_player = p' + t', _tvel = t', _air = a'}
  where
    t1 = (v + V2 0 1 +) $ t & _x %~ (* 0.7)
    (p', t', a') = foldr block (p, t1, a) tilesr

block :: Rectangle -> (Vector2, Vector2, Bool) -> (Vector2, Vector2, Bool)
block r@(Rectangle rx ry rw rh) t@(V2 px py, V2 tx ty, _) =
  traceShowWith (r, t,) $
  (if inside_h -- down (w/ floor)
  && py + pr <= ry && py + ty + pr >= ry
  then (_1._y .~ ry) . (_2._y .~ 0) . (_3 .~ False) else id) $ 
  (if inside_h -- up
  && py - pr >= ry + rh && py + ty - pr <= ry + rh
  then (_1._y .~ ry + rh) . (_2._y .~ 0) else id) $
  (if inside_v -- right
  && px + pr <= rx && px + tx + pr >= rx
  then (_1._x .~ rx) . (_2._x .~ 0) else id) $
  (if inside_v -- left
  && px - pr >= rx + rw && px + tx - pr <= rx + rw
  then (_1._x .~ rx + rw) . (_2._x .~ 0) else id)
  t
  where
    pr = playerRad
    inside_h = px + pr >= rx && px - pr <= rx + rw
    inside_v = py + pr >= ry && py - pr <= ry + rh
