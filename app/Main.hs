module Main (main) where

import Raylib.Core as R
import Raylib.Core.Textures as Texture

import Control.Monad.State ( evalStateT )
import Lib ( tilClose, init', tick )

main :: IO ()
main = do
  setTargetFPS 30
  window <- initWindow 1000 1000 "hello"
  init'' <- init'
  evalStateT (tilClose tick) init''
  closeWindow (Just window)
