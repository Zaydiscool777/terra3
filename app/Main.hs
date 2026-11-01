module Main (main) where

import Raylib.Core as R (setTargetFPS, initWindow, closeWindow)

import Control.Monad.State (evalStateT)
import Lib (init', tick, tilClose)

main :: IO ()
main = do
  setTargetFPS 30
  window <- initWindow 1000 1000 "hello"
  init'' <- init'
  evalStateT (tilClose tick) init''
  closeWindow (Just window)
