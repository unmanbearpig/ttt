{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import qualified Data.Text as T
import Text.Printf (printf)
-- import qualified Data.Array as A
-- import qualified Data.Vector as V
import System.IO
import Control.Lens
import Control.Monad

data Player = Player { name :: String, char :: Char } deriving (Show)
makeLenses ''Player


data Coordinates = Coordinates Int deriving (Eq)

mapC :: (Int -> Int) -> Coordinates -> Coordinates
mapC f (Coordinates x) = Coordinates (f x)
moveC :: Int -> Coordinates -> Coordinates
moveC x = mapC (+ x)

data Board = Board { _size :: Int
                   , _playerCoordinates :: Coordinates
                   , _player :: Player }
makeLenses ''Board

data Command = Move Int | MoveBackward | NoOp

parse :: Char -> Command
parse 'n' = Move 1
parse 'p' = Move (-1)
parse _   = NoOp

renderPlayer :: Player -> Char
renderPlayer = char

listBoardCoordinates :: Board -> [Coordinates]
listBoardCoordinates b = map Coordinates [0..(view size b)-1]

newBoard :: Int -> Board
newBoard size = Board size (Coordinates 0) (Player "blah" '@')

queryBoard :: Board -> Coordinates -> Maybe Player
queryBoard b c = if c == view playerCoordinates b then Just $ view player b else Nothing

boardChars :: Board -> String
boardChars b = map (maybe '.' renderPlayer) $ map (queryBoard b) $ listBoardCoordinates b

renderBoard :: Board -> String
renderBoard b = "|" ++ boardChars b ++ "|"

processCommand :: Board -> Command -> Board
processCommand b (Move x) = over playerCoordinates (moveC x) b
processCommand b _ = b

getCommand :: IO Command
getCommand = getChar >>= \c -> return $ parse c

printBoard :: Board -> IO ()
printBoard = putStrLn . renderBoard

doATurn :: Board -> IO Board
doATurn b = getCommand
            >>= (\c -> return $ processCommand b c)
            >>= (\nb -> printBoard nb >> return nb)


main :: IO ()
main = hSetBuffering stdin NoBuffering
       >> hSetEcho stdin False
       >> return initialBoard
       >>= \b -> foldM_ ((flip . const) doATurn) b [0..]

  where
    initialBoard = (newBoard size)
    size = 70
