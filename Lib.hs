{-# LANGUAGE TupleSections, BlockArguments, LexicalNegation #-}
module Lib ( init', tick, tilClose ) where

import Raylib.Core as R
-- import Raylib.Core.Audio as Audio
-- import Raylib.Core.Models as Model
import Raylib.Core.Shapes as Shape
-- import Raylib.Core.Text as Text
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
import Data.Function

-- utils

-- untilM :: Monad m => (a -> Bool) -> m a -> m a from: utility-ht
loopUntil :: Monad m => (a -> Bool) -> StateT s m a -> s -> m [a]
loopUntil p m s = do
  (x, s') <- runStateT m s
  if p x then return [] else (x :) <$> loopUntil p m s'

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

-- elm

data Model = Model {
  player :: Vector2,
  uvel :: Vector2,
  tvel :: Vector2,
  air :: Bool
} deriving Show

init' :: Model
init' = Model {
  player = pure 201,
  uvel = pure 0,
  tvel = pure 0,
  air = True
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
upd m AStart = return m {uvel = pure 0}
upd m (AKey KeyUp) = return m {uvel = uvel m & if air m then id else _y %~ (- 30)}
upd m (AKey KeyDown) = return m {uvel = uvel m & _y %~ (+ 10)}
upd m (AKey KeyLeft) = return m {uvel = uvel m & _x %~ (- 10)}
upd m (AKey KeyRight) = return m {uvel = uvel m & _x %~ (+ 10)}
upd _ (AKey KeyR) = return init'
upd m AEnd = return (move m)
upd m _ = return m

view' :: Model -> IO ()
view' m = do
  beginDrawing
  clearBackground rayWhite
  w <- getScreenWidth
  h <- getScreenHeight
  beginMode2D (center (player m) w h)
  drawtiles
  drawplayer (player m)
  endMode2D
  endDrawing -- presents frame to user
  waitTime 0.03

-- io

drawtiles :: IO ()
drawtiles = mapM_ (`drawRectangleRec` black) tilesr

drawplayer :: Vector2 -> IO ()
drawplayer v = drawCircleV v 40 blue

-- pure

tiles :: [[Integer]]
tiles = fmap (fmap (bool 0 1 . (== '1'))) [
  "",
  "111111111111",
  "",
  "111100001111"]

tilesr' :: Float -> [Rectangle]
tilesr' k = (\(row, col) -> Rectangle (col * k) (row * k) k k) <$> (m >>= catMaybes)
  where m = zipWith (\i r -> zipWith (\j x -> guard (x == 1) >> Just (i, j)) [0..] r) [0..] tiles

-- playerRadius :: Float
-- playerRadius = 10

tilesr :: [Rectangle]
tilesr = tilesr' 100 -- [Rectangle 100 100 400 125]

center :: Vector2 -> Int -> Int -> Camera2D
center v c d = Camera2D ((/ 2) . fromIntegral <$> V2 c d) v 0 1

move :: Model -> Model
move m@Model {player = p, uvel = v, tvel = t} =
  m {player = p + t', tvel = t', air = t1 == t'}
  where
    t1 =  (v + V2 0 1 +) $ t & _x %~ (* 0.7)
    t' = foldr (block . (p,)) t1 tilesr

block :: (Vector2, Rectangle) -> Vector2 -> Vector2
block (V2 px py, Rectangle rx ry rw rh) t@(V2 tx ty) =
  (if px > rx && px < rx + rw
  && py <= ry && py + ty >= ry
  then _y .~ ry - py else id) $
  (if px > rx && px < rx + rw
  && py >= ry + rh && py + ty <= ry + rh
  then _y .~ ry + rh - py else id) $
  (if py > ry && py < ry + rh
  && px <= rx && px + tx >= rx
  then _x .~ rx - px else id) $
  (if py > ry && py < ry + rh
  && px >= rx + rw && px + tx <= rx + rw
  then _x .~ rx + rw - px else id)
  t
