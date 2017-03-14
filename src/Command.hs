module Command where

import Coordinates

data Command = Move Coordinates | NoOp

parse :: Char -> Command
parse 'f' = Move $ Coordinates 0 1
parse 'b' = Move $ Coordinates 0 (-1)
parse 'n' = Move $ Coordinates 1 0
parse 'p' = Move $ Coordinates (-1) 0
parse _   = NoOp

getCommand :: IO Command
getCommand = getChar >>= \c -> return $ parse c
