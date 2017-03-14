{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( Coordinates(..), newBoard, printBoard, doATurn
    ) where

import qualified Data.Text as T
import Data.List
import Command
import Coordinates
import System.IO
import Control.Lens
import Control.Monad
import Control.Applicative

data Player = Player { _name :: String, _char :: Char, _playerCoordinates :: Coordinates } deriving (Show)
makeLenses ''Player

data Board = Board { _size :: Coordinates
                   , _player :: Player }
makeLenses ''Board

renderPlayer :: Player -> Coordinates -> Maybe Char
renderPlayer p c
  | p^.playerCoordinates == c = Just (p^.char)
  | otherwise  = Nothing

listBoardCoordinates :: Board -> [Coordinates]
listBoardCoordinates b = liftA2 Coordinates [0..(b^.size.xC-1)] [0..(b^.size.yC)-1]

newBoard :: Coordinates -> Board
newBoard size = Board size (Player "blah" '@' (Coordinates 0 0))

boardRows :: Board -> [String]
boardRows b = map (map (renderBoardCell b)) $ groupBy (\ac bc -> ac^.xC == bc^.xC) $ listBoardCoordinates b

renderBoardCell :: Board -> Coordinates -> Char
renderBoardCell b c = maybe ' ' id $ renderPlayer (b^.player) c

renderBoard :: Board -> String
renderBoard b = join $ intersperse "\n" ([horizontalBorder] ++ rows ++ [horizontalBorder])
  where rows = map (\str -> "|" ++ str ++ "|") $ boardRows b :: [String]
        horizontalBorder = replicate (b^.size.yC + 2) '-'

processCommand :: Board -> Command -> Board
processCommand b (Move c) = over (player.playerCoordinates) (+%+ c) b
processCommand b _ = b

printBoard :: Board -> IO ()
printBoard b = putStr "\ESC[0;0f" >> (putStrLn . renderBoard) b

doATurn :: Board -> IO Board
doATurn b = getCommand
            >>= (\c -> return $ processCommand b c)
            >>= (\nb -> printBoard nb >> return nb)
