module Main (main) where

import Raylib.Core as R

import Control.Monad.State ( evalStateT )
import Lib ( tilClose, init', tick, test )

main :: IO ()
main = print test
main' = do
  window <- initWindow 1000 1000 "hello"
  evalStateT (tilClose tick) init'
  closeWindow (Just window)
