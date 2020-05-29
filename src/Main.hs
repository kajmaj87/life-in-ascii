{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import           Apecs
import           Linear                         ( V2(..) )
import           Control.Monad
import qualified System.Terminal               as T

newtype Position = Position (V2 Int) deriving Show
newtype Velocity = Velocity (V2 Int) deriving Show
newtype Tile = Tile Char deriving Show
data Flying = Flying


makeWorldAndComponents "World" [''Position, ''Tile, ''Velocity, ''Flying]

initialise :: System World ()
initialise = do
  newEntity (Position 0, Tile 'v', Velocity 1)
  newEntity (Position 2, Tile 'w', Velocity 1)
  newEntity (Position 1, Tile 'f', Velocity 2, Flying)

  -- 1. Add velocity to position
  -- 2. Apply gravity to non-flying entities
  -- 3. Print a list of entities and their positions
  -- cmap $ \(Position p, Velocity v) -> Position (v + p)
  -- cmap $ \(Velocity v, _ :: Not Flying) -> Velocity (v - V2 0 1)
  -- cmapM_ $ \(Position p, Entity e) -> liftIO . print $ (e, p)
  return ()

step :: System World ()
step = cmap $ \(Position p, Velocity v) -> Position (v + p)

draw :: System World ()
draw =
  cmapM_ $ \(Position (V2 x y), Tile c) -> liftIO (drawCharOnTerminal c x y)

drawChar :: T.MonadScreen m => Char -> Int -> Int -> m ()
drawChar c x y = do
  T.setCursorPosition $ T.Position y x
  T.putChar c

loop :: World -> Int -> IO ()
loop world turns = do
  runSystem step world
  runSystem draw world
  onTerminal T.flush

  when (turns > 0) $ loop world (turns - 1)

drawCharOnTerminal c x y = onTerminal (drawChar c x y)

onTerminal f = T.withTerminal $ T.runTerminalT $ f


main :: IO ()
main = do
  world <- initWorld
  runSystem initialise world
  loop world 10
