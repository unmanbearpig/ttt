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

data Command = MoveForward | MoveBackward | NoOp

parse :: Char -> Command
parse 'n' = MoveForward
parse 'p' = MoveBackward
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

executeCommand :: Board -> Command -> Board
executeCommand b MoveForward = over playerCoordinates (moveC 1) b
executeCommand b MoveBackward = over playerCoordinates (moveC (-1)) b
executeCommand b _ = b

getCommand :: IO Command
getCommand = getChar >>= \c -> return $ parse c

printBoard :: Board -> IO ()
printBoard = putStrLn . renderBoard

doATurn :: Board -> IO Board
doATurn b = getCommand
            >>= (\c -> return $ executeCommand b c)
            >>= (\nb -> printBoard nb >> return nb)


main :: IO ()
main = hSetBuffering stdin NoBuffering
       >> hSetEcho stdin False
       >> return (newBoard size)
       >>= \b -> foldM_ (\bb _ -> doATurn bb) b [0..]

       -- >>= \board -> (putStrLn $ renderBoard board)
       -- >>= \board -> getChar >>= \c -> snd $ play c (board, putStrLn "fuc")
  where size = 70
