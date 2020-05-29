{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
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
import           System.Random

newtype Position = Position (V2 Int) deriving Show
newtype Tile = Tile Char deriving Show
newtype TTL = TTL Int
data Growing = Growing Int Int
data Split = Split
data Alive = Alive
data Solid = Solid


makeWorldAndComponents "World" [''Position, ''Tile, ''Solid, ''TTL, ''Alive, ''Growing, ''Split]

initialise :: System World ()
initialise = do
  room 0 1 40 20
  grass 10 10
  grass 20 15
  -- 1. Add velocity to position
  -- 2. Apply gravity to non-flying entities
  -- 3. Print a list of entities and their positions
  -- cmap $ \(Position p, Velocity v) -> Position (v + p)
  -- cmap $ \(Velocity v, _ :: Not Flying) -> Velocity (v - V2 0 1)
  -- cmapM_ $ \(Position p, Entity e) -> liftIO . print $ (e, p)
  return ()

grass x y = newEntity (Position (V2 x y), Tile '.', Growing 10 1, TTL 50)

wall x y = newEntity (Position (V2 x y), Tile '#', Solid)
--room :: Int -> Int -> Int -> Int -> SystemT w m ()
room xmin ymin xmax ymax = mapM_
  (uncurry wall)
  (  [ (x, y) | x <- [xmin, xmax], y <- [ymin .. ymax] ]
  ++ [ (x, y) | x <- [(xmin + 1) .. (xmax - 1)], y <- [ymin, ymax] ]
  )

step :: System World ()
step = do
  -- get older
  cmap $ \(TTL turns, Tile c) ->
    if turns > 0 then (TTL $ turns - 1, Tile c) else (TTL 0, Tile '!')
  -- grow 
  cmap $ \(Growing period current) -> Growing period (current + 1)
  cmapM $ \(Position (V2 x y), Growing period current) ->
    if current `mod` period == 0
      then do
      --  newEntity (Position (V2 (x + 1) (y + 1)), Growing period (current + 1))
           --pure (Position (V2 (x + 1) y), Growing period current)
        dx <- liftIO $ randomRIO (-1, 1)
        dy <- liftIO $ randomRIO (-1, 1)
        _  <- grass (x + dx) (y + dy)
        pure (Position (V2 x y), Growing period current)
      else do
        pure (Position (V2 x y), Growing period current)


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
  onTerminal T.hideCursor
  runSystem initialise world
  loop world 100
