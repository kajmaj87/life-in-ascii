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

data Moving = Moving
instance Component Moving where
  type Storage Moving = Map Moving

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
  mempty = VisibleTiles M.empty
instance Component VisibleTiles where
  type Storage VisibleTiles = Global VisibleTiles

newtype Time = Time Float deriving (Show, Num)
instance Semigroup Time where
  (<>) = (+)
instance Monoid Time where
  mempty = 0
instance Component Time where
  type Storage Time = Global Time

newtype Log = Log [String] deriving (Show)
instance Semigroup Log where
  (Log a) <> (Log b) = Log (a ++ b)
instance Monoid Log where
  mempty = Log []
instance Component Log where
  type Storage Log = Global Log

type All = (Position, Tile, Solid, Physical, TTL, Growing, Moving)

data Direction = Horizontal | Vertical

growthRate :: Int
growthRate = 25

grassMaxAge = 50
bunnyMaxAge = 500

makeWorld "World" [''Position, ''Tile, ''Solid, ''Physical, ''TTL, ''Growing, ''Moving, ''Time, ''VisibleTiles, ''Log]

initialise :: System World ()
initialise = do
  room 0 1 40 20
  emptyFloor 1 2 39 19
  -- _ <- grass 15 13 0
  _ <- grass 35 3 0
  bunny 14 12
  wall 10 10 10 Horizontal
  wall 10 15 10 Horizontal
  wall 10 11 2  Vertical
  wall 20 11 3  Vertical
  return ()

grass x y p = newEntity
  ( Position (V2 x y)
  , Tile '.' 1
  , Growing growthRate p
  , TTL grassMaxAge
  , Physical
  )

bunny x y =
  newEntity (Position (V2 x y), Tile 'b' 2, Physical, TTL bunnyMaxAge, Moving)

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

emptyFloor xmin ymin xmax ymax = mapM_
  (uncurry emptySpace)
  [ (x, y) | x <- [xmin .. xmax], y <- [ymin .. ymax] ]

step :: World -> System World ()
step world = do
  modify global (\(Time turns) -> Time (turns + 1))
  -- get older and die eventually
  cmap $ \(TTL turns) ->
    if turns < 0 then Right $ Not @All else Left $ TTL (turns - 1)
  liftIO $ runSystem grow world
  return ()

move :: System World ()
move = cmapM $ \(Moving, Position p) -> do
  newPosition <- moveIfPossible (Position p)
  return (Moving, newPosition)

-- returns current position if move impossible
moveIfPossible :: Position -> System World Position
moveIfPossible (Position (V2 x y)) = do
  dx            <- liftIO $ randomRIO (-1, 1)
  dy            <- liftIO $ randomRIO (-1, 1)
  spaceOccupied <- cfold
    (\acc (Position (V2 px py), Physical) ->
      acc || (px == (x + dx) && py == (y + dy))
    )
    False
  if spaceOccupied
    then return (Position (V2 x y))
    else return (Position (V2 (x + dx) (y + dy)))

grow :: System World ()
grow = do
  -- grow 
  cmap $ \(Growing period current) -> Growing period (current + 1)
  cmapM $ \(Position p, Growing period current) -> if current `mod` period == 0
    then do
      Position (V2 x' y') <- moveIfPossible (Position p)
      randomPeriod        <- liftIO $ randomRIO (0, growthRate)
      unless
        (Position (V2 x' y') == Position p)
        (do
          _ <- grass x' y' randomPeriod
          return ()
        )
      pure (Position p, Growing period current)
    else pure (Position p, Growing period current)


updateVisibleMap :: System World ()
updateVisibleMap = do
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

  changedTiles <- return
    (M.differenceWith (\new old -> if old == new then Nothing else Just new)
                      newTiles
                      oldTiles
    )

  addLog
    $  "New/Old/Total changed tiles: "
    ++ show (length newTiles)
    ++ "/"
    ++ show (length oldTiles)
    ++ "/"
    ++ show (length changedTiles)
  mapM_
    (\((Position (V2 x y)), (Tile c _)) -> liftIO (drawCharOnTerminal c x y))
    (M.toList changedTiles)

addLog :: String -> System World ()
addLog s = modify global (\(Log xs) -> Log (s : xs))

logging :: System World ()
logging = do
  Time turn <- get global
  entities  <- cfold (\acc (Tile _ _) -> acc + 1) 0
  addLog ("Current turn: " ++ show turn ++ " Entities: " ++ show entities)
  Log logs <- get global
  case logs of
    x : _ -> do
      liftIO (onTerminal (drawString x 1 22))
      return ()
    [] -> return ()

drawChar :: T.MonadScreen m => Char -> Int -> Int -> m ()
drawChar c x y = do
  T.setCursorPosition $ T.Position y x
  T.putChar c

drawString :: T.MonadScreen m => String -> Int -> Int -> m ()
drawString s x y = do
  T.setCursorPosition $ T.Position y x
  T.putStringLn s

loop :: World -> Int -> IO ()
loop world turns = do
  runSystem (step world)     world
  runSystem move             world
  runSystem updateVisibleMap world
  runSystem logging          world
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
  drawCharOnTerminal 'E' 1 22
