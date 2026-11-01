{-# LANGUAGE LexicalNegation, TemplateHaskell, MultiWayIf #-}
module Lib (init', tick, tilClose) where

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
import qualified Data.Map as M
import Data.Foldable

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
  AStart | AEnd
  | AKey KeyboardKey

-- elm

init' :: IO Model
init' = do
  img0 <- squaretexture (playerRad * 2) "./ball.png"
  img1 <- squaretexture tilesLen "./wall.png"
  img2 <- squaretexture tilesLen "./portal.png"
  return Model {
    _player = Player {
      _pos = pure 200,
      _uvel = pure 0,
      _tvel = pure 0,
      _air = True
    },
    _image = (
      img0,
      M.fromList [
        ('1', img1),
        ('2', img2)
      ]),
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
  x <- getKeysDown
  let i1 = upd i AStart
  let i2 = foldr (flip upd . AKey) i1 x -- thanks to https://discord.com/users/1276891525451743295
  return $ upd i2 AEnd

upd :: Model -> Action -> Model
upd m AStart = m & player.uvel .~ pure 0
upd m (AKey KeyUp) = m
  & player.air .~ True
  & player.uvel %~ (+ V2 0 (if m^.player.air then 0 else -22))
upd m (AKey KeyDown) = m & player.uvel %~ (+ V2 0 10)
upd m (AKey KeyLeft) = m & player.uvel %~ (- V2 10 0)
upd m (AKey KeyRight) = m & player.uvel %~ (+ V2 10 0)
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
  drawtiles ( tiles (m^.lvlnum)) (m^.image._2)
  drawplayer (m^.player.pos) (m^.image._1)
  endMode2D
  endDrawing -- presents frame to user
  drawText (unlines $ chunksOf 25 $ show $ m^.player) 0 100 24 darkGray

-- io

squaretexture :: Int -> String -> IO Texture
squaretexture l = loadImage >=> flip (`imageResize` l) l >=> loadTextureFromImage

-- thanks to https://discord.com/users/1276891525451743295
drawtiles :: [(Char, Rectangle)] -> M.Map Char Texture -> IO ()
drawtiles t i =
  for_ t (\(id', x) ->
    for_ (M.lookup id' i) (\i' ->
      drawTextureV i' (V2 (x^._rectangle'x) (x^._rectangle'y)) white))

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
    (p', t', a') = foldr block (p, t1, a) [y | (x, y) <-  tiles l, x == '1']

block :: Rectangle -> (Vector2, Vector2, Bool) -> (Vector2, Vector2, Bool)
block (Rectangle rx ry rw rh) t@(V2 px py, V2 tx ty, _) = (blockv . blockh) t
  where
    pr = playerRad
    -- thanks to https://discord.com/users/1276891525451743295
    blockv = if
      | not (px + pr > rx && px - pr < rx + rw) -> id
      | py + pr <= ry && py + ty + pr >= ry
        -> (_1._y .~ ry - pr) . (_2._y .~ 0) . (_3 .~ False)
      | py - pr >= ry + rh && py + ty - pr <= ry + rh
        -> (_1._y .~ ry + rh + pr) . (_2._y .~ 0)
      | otherwise -> id
    blockh = if
      | not (py + pr > ry && py - pr < ry + rh) -> id
      | px + pr <= rx && px + tx + pr >= rx
        -> (_1._x .~ rx - pr) . (_2._x .~ 0)
      | px - pr >= rx + rw && px + tx - pr <= rx + rw
        -> (_1._x .~ rx + rw + pr) . (_2._x .~ 0)
      | otherwise -> id

inputted :: Model -> Model
inputted m = m
  & player %~ move (m^.lvlnum)
  & lvlnum %~ chlvl (m^.player.pos)

chlvl :: Vector2 -> Int -> Int
chlvl p l = if any ((== '2') . fst) at_blocks then succ l else l
  where
    at_blocks = [b | b@(_, x) <- tiles l, V2 (x^._rectangle'x) (x^._rectangle'y) == rounded p]
rounded :: Vector2 -> Vector2
rounded p = (fromIntegral :: Int -> Float) . (* 100) . (`div` 100) . floor <$> p
-- ('2', Rectangle {rectangle'x = 1000.0, rectangle'y = 400.0, rectangle'width = 100.0, rectangle'height = 100.0})

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

tiles :: Int -> [(Char, Rectangle)]
tiles i = (\(id', col, row) -> (id', Rectangle (col * k) (row * k) k k)) <$> tiles' !! i
  where
    k = tilesLen
    tiles' = map (makeTiles . lines)
      [
        "       1      \n\
        \1 11111111111 \n\
        \1             \n\
        \111111   11111\n\
        \          2   \n\
        \   111111111  ",
        
        "              \n\
        \1 11 11   111 \n\
        \   2   1      \n\
        \11  11  11   1\n\
        \              \n\
        \   11  1 11   "
      ]

makeTiles :: [String] -> [(Char, Float, Float)]
makeTiles = join . zipWith (\i r -> zipWith (\j x -> (x, j, i)) [0..] r) [0..]

playerRad, tilesLen :: Num a => a
playerRad = 15
tilesLen = 100
