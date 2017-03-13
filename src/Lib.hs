{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( Coordinates(..), newBoard, printBoard, doATurn
    ) where

import qualified Data.Text as T
import Data.List
import Text.Printf (printf)
import System.IO
import Control.Lens
import Control.Monad
import Control.Applicative

data Player = Player { name :: String, char :: Char } deriving (Show)
makeLenses ''Player

data Coordinates = Coordinates { _xC :: Int, _yC :: Int } deriving (Eq)
makeLenses ''Coordinates

(+%+) :: Coordinates -> Coordinates -> Coordinates
a +%+ b = Coordinates (a^.xC + b^.xC) (a^.yC + b^.yC)

data Board = Board { _size :: Coordinates
                   , _playerCoordinates :: Coordinates
                   , _player :: Player }
makeLenses ''Board

data Command = Move Coordinates | MoveBackward | NoOp

parse :: Char -> Command
parse 'f' = Move $ Coordinates 0 1
parse 'b' = Move $ Coordinates 0 (-1)
parse 'n' = Move $ Coordinates 1 0
parse 'p' = Move $ Coordinates (-1) 0
parse _   = NoOp

renderPlayer :: Player -> Char
renderPlayer = char

listBoardCoordinates :: Board -> [Coordinates]
listBoardCoordinates b = liftA2 Coordinates [0..(b^.size.xC-1)] [0..(b^.size.yC)-1]

newBoard :: Coordinates -> Board
newBoard size = Board size (Coordinates 0 0) (Player "blah" '@')

queryBoard :: Board -> Coordinates -> Maybe Player
queryBoard b c = if c == b^.playerCoordinates then Just $ b^.player else Nothing

boardRows :: Board -> [String]
boardRows b = map (map (renderBoardCell b)) $ groupBy (\ac bc -> ac^.xC == bc^.xC) $ listBoardCoordinates b

renderBoardCell :: Board -> Coordinates -> Char
renderBoardCell b c = maybe ' ' renderPlayer $ queryBoard b c

renderBoard :: Board -> String
renderBoard b = join $ intersperse "\n" ([horizontalBorder] ++ rows ++ [horizontalBorder])
  where rows = map (\str -> "|" ++ str ++ "|") $ boardRows b :: [String]
        horizontalBorder = replicate (b^.size.yC + 2) '-'

processCommand :: Board -> Command -> Board
processCommand b (Move c) = over (playerCoordinates) (+%+ c) b
processCommand b _ = b

getCommand :: IO Command
getCommand = getChar >>= \c -> return $ parse c

printBoard :: Board -> IO ()
printBoard = putStrLn . renderBoard

doATurn :: Board -> IO Board
doATurn b = getCommand
            >>= (\c -> return $ processCommand b c)
            >>= (\nb -> printBoard nb >> return nb)
