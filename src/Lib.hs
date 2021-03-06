{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

module Lib
    ( Coordinates(..), newBoard, printBoard, play
    ) where

import qualified Data.Text as T
import Data.List
import Command
import Coordinates
import System.IO
import Control.Lens
import Control.Monad
import Control.Applicative
import Data.Monoid

data Pixel = Transparent | Solid Char deriving Show

instance Monoid Pixel where
  mempty = Transparent
  mappend Transparent y  = y
  mappend x Transparent  = x
  mappend x _            = x
  mconcat ps = foldr mappend mempty ps

pixelChar :: Pixel -> Char
pixelChar (Solid c) = c
pixelChar Transparent = ' '


class GameObject t where
  render :: t -> Coordinates -> Pixel

data GameObjInstance = forall a. GameObject a => GO a
instance GameObject GameObjInstance where
  render (GO go) c = render go c

mkGameObj :: GameObject a => a -> GameObjInstance
mkGameObj = GO

data Player = Player { _name :: String, _playerPixel :: Pixel, _playerCoordinates :: Coordinates } deriving (Show)
makeLenses ''Player
instance GameObject Player where
  render p c
    | p^.playerCoordinates == c = p^.playerPixel
    | otherwise  = Transparent


data Monster = Monster { _monsterPixel :: Pixel, _monsterCoordinates :: Coordinates }
makeLenses ''Monster
instance GameObject Monster where
  render m c
    | m^.monsterCoordinates == c = m^.monsterPixel
    | otherwise = Transparent

data Board = Board { _size :: Coordinates
                   , _player :: Player
                   , _gameObjects :: [GameObjInstance] }
makeLenses ''Board

listBoardCoordinates :: Board -> [Coordinates]
listBoardCoordinates b = liftA2 Coordinates [0..(b^.size.xC-1)] [0..(b^.size.yC)-1]

newBoard :: Coordinates -> Board
newBoard size = Board size (Player "blah" (Solid '@') (Coordinates 0 0)) [(mkGameObj $ Monster (Solid '%') (Coordinates 20 20))]

boardRows :: Board -> [String]
boardRows b = map (map (renderBoardCell b)) $ groupBy (\ac bc -> ac^.xC == bc^.xC) $ listBoardCoordinates b

getBoardPixels :: Board -> Coordinates -> [Pixel]
getBoardPixels b c = (render (b^.player) c) : (map (\o -> render o c) $ b^.gameObjects)

renderBoardCell :: Board -> Coordinates -> Char
renderBoardCell b c = pixelChar $ mconcat $ getBoardPixels b c

renderBoard :: Board -> String
renderBoard b = join $ intersperse "\n" ([horizontalBorder] ++ rows ++ [horizontalBorder])
  where rows = map (\str -> "|" ++ str ++ "|") $ boardRows b :: [String]
        horizontalBorder = replicate (b^.size.yC + 2) '-'

processCommand :: Board -> Command -> Board
processCommand b (Move c) = over (player.playerCoordinates) (+%+ c) b
processCommand b _ = b

printBoard :: Board -> IO ()
printBoard b = putStr "\ESC[0;0f" >> (putStrLn . renderBoard) b

play :: Board -> IO Board
play b = printBoard b >> processCommand b <$> getCommand >>= play
