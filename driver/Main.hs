module Main
  ( main
  ) where

import Prelude

import Module.Discover.Run (run)
import System.Environment

main :: IO ()
main = getArgs >>= run
