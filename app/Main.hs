module Main where

import Control.Monad
import System.IO

import Lib
main :: IO ()
main = hSetBuffering stdin NoBuffering
       >> hSetEcho stdin False
       >> printBoard initialBoard
       >> return initialBoard
       >>= \b -> foldM_ ((flip . const) doATurn) b [0..]

  where
    initialBoard = (newBoard size)
    size = Coordinates 50 80
