{-# LANGUAGE TemplateHaskell #-}

module Coordinates where

import Control.Lens

data Coordinates = Coordinates { _xC :: Int, _yC :: Int } deriving (Eq)
makeLenses ''Coordinates

(+%+) :: Coordinates -> Coordinates -> Coordinates
a +%+ b = Coordinates (a^.xC + b^.xC) (a^.yC + b^.yC)
