{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import           Apecs
import           Linear                         ( V2(..) )
import           Control.Monad
import qualified System.Terminal               as T
import qualified Data.Map                      as M
import           System.Random

newtype Position = Position (V2 Int) deriving (Show, Ord, Eq)
instance Component Position where
  type Storage Position = Map Position

data Tile = Tile Char Int deriving (Show, Eq)
instance Component Tile where
  type Storage Tile = Map Tile

newtype TTL = TTL Int
instance Component TTL where
  type Storage TTL = Map TTL
data Growing = Growing Int Int
instance Component Growing where
  type Storage Growing = Map Growing
data Solid = Solid
instance Component Solid where
  type Storage Solid = Map Solid
data Physical = Physical
instance Component Physical where
  type Storage Physical = Map Physical

newtype VisibleTiles = VisibleTiles (M.Map Position Tile)
instance Semigroup VisibleTiles where
  (VisibleTiles a) <> (VisibleTiles b) = VisibleTiles (M.union a b)
instance Monoid VisibleTiles where
  mempty = VisibleTiles (M.empty)
instance Component VisibleTiles where
  type Storage VisibleTiles = Global VisibleTiles

newtype Time = Time Float deriving (Show, Num)
instance Semigroup Time where
  (<>) = (+)
instance Monoid Time where
  mempty = 0
instance Component Time where
  type Storage Time = Global Time

type All = (Position, Tile, Solid, Physical, TTL, Growing)

data Direction = Horizontal | Vertical

makeWorld "World" [''Position, ''Tile, ''Solid, ''Physical, ''TTL, ''Growing, ''Time, ''VisibleTiles]

initialise :: System World ()
initialise = do
  room 0 1 40 20
  emptyFloor 1 2 39 19
  grass 15 13
  wall 10 10 10 Horizontal
  wall 10 15 10 Horizontal
  wall 10 11 3  Vertical
  wall 20 11 3  Vertical
  -- 1. Add velocity to position
  -- 2. Apply gravity to non-flying entities
  -- 3. Print a list of entities and their positions
  -- cmap $ \(Position p, Velocity v) -> Position (v + p)
  -- cmap $ \(Velocity v, _ :: Not Flying) -> Velocity (v - V2 0 1)
  -- cmapM_ $ \(Position p, Entity e) -> liftIO . print $ (e, p)
  return ()

grass x y =
  newEntity (Position (V2 x y), Tile '.' 1, Growing 15 0, TTL 50, Physical)

brick x y = newEntity (Position (V2 x y), Tile '#' 10, Solid, Physical)
wall xs ys n Horizontal =
  mapM_ (uncurry brick) [ (x, ys) | x <- [xs .. (xs + n)] ]
wall xs ys n Vertical =
  mapM_ (uncurry brick) [ (xs, y) | y <- [ys .. (ys + n)] ]

emptySpace x y = newEntity (Position (V2 x y), Tile ' ' 0)

room xmin ymin xmax ymax = mapM_
  (uncurry brick)
  (  [ (x, y) | x <- [xmin, xmax], y <- [ymin .. ymax] ]
  ++ [ (x, y) | x <- [(xmin + 1) .. (xmax - 1)], y <- [ymin, ymax] ]
  )

emptyFloor xmin ymin xmax ymax =
  mapM_ (uncurry emptySpace) [ (x, y) | x <- [xmin, xmax], y <- [ymin .. ymax] ]

step :: System World ()
step = do
  -- get older and die eventually
  cmap $ \(TTL turns) ->
    if turns < 0 then Right $ Not @All else Left $ TTL (turns - 1)
  -- grow 
  cmap $ \(Growing period current) -> Growing period (current + 1)
  cmapM $ \(Position (V2 x y), Growing period current) ->
    if current `mod` period == 0
      then do
      --  newEntity (Position (V2 (x + 1) (y + 1)), Growing period (current + 1))
           --pure (Position (V2 (x + 1) y), Growing period current)
        dx            <- liftIO $ randomRIO (-1, 1)
        dy            <- liftIO $ randomRIO (-1, 1)
        spaceOccupied <- cfold
          (\acc (Position (V2 px py), Physical) ->
            acc || (px == (x + dx) && py == (y + dy))
          )
          False
        if spaceOccupied
          then do
            return ()
          else do
            grass (x + dx) (y + dy)
            return ()
        pure (Position (V2 x y), Growing period current)
      else do
        pure (Position (V2 x y), Growing period current)


updateVisibleMap :: System World ()
updateVisibleMap = do
  --result <- cmapM_
   -- $ \(Position (V2 x y), Tile c z) -> liftIO (drawCharOnTerminal c x y)
  --cfold $ (\(VisibleTiles acc) (Position p, Tile c z) -> VisibleTiles (M.insert (Position p) (Tile c z) acc)) mempty
  VisibleTiles newTiles <- cfold
    (\(VisibleTiles acc) (Position p, Tile c z) -> VisibleTiles
      (M.insertWith
        (\(Tile c1 z1) (Tile c2 z2) ->
          if z1 > z2 then Tile c1 z1 else Tile c2 z2
        )
        (Position p)
        (Tile c z)
        acc
      )
    )
    mempty

  VisibleTiles oldTiles <- get global
  set global (VisibleTiles newTiles)

  mapM_
    (\((Position (V2 x y)), (Tile c z)) -> liftIO (drawCharOnTerminal c x y))
    (M.toList
      (M.differenceWith (\new old -> if old == new then Nothing else Just new)
                        newTiles
                        oldTiles
      )
    )

  return ()


drawChar :: T.MonadScreen m => Char -> Int -> Int -> m ()
drawChar c x y = do
  T.setCursorPosition $ T.Position y x
  T.putChar c

loop :: World -> Int -> IO ()
loop world turns = do
  runSystem step             world
  runSystem updateVisibleMap world
  onTerminal T.flush

  when (turns > 0) $ loop world (turns - 1)

drawCharOnTerminal c x y = onTerminal (drawChar c x y)

onTerminal f = T.withTerminal $ T.runTerminalT $ f


main :: IO ()
main = do
  world <- initWorld
  onTerminal T.hideCursor
  onTerminal $ T.eraseInDisplay T.EraseAll
  runSystem initialise world
  loop world 1000
  drawCharOnTerminal 'E' 1 21
