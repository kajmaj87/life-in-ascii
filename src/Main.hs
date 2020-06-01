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
import           Control.Monad.IO.Class
import           Control.Monad.Catch
import qualified System.Terminal               as T
import           System.Terminal.Internal
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

newtype Eats = Eats FoodType
instance Component Eats where
  type Storage Eats = Map Eats

data IsFood = IsFood FoodType Int
instance Component IsFood where
  type Storage IsFood = Map IsFood

newtype Energy = Energy Int
instance Component Energy where
  type Storage Energy = Map Energy

data Moving = Moving
instance Component Moving where
  type Storage Moving = Map Moving

data Solid = Solid
instance Component Solid where
  type Storage Solid = Map Solid

data Physical = Physical
instance Component Physical where
  type Storage Physical = Map Physical

newtype Spawns = Spawns Int
instance Component Spawns where
  type Storage Spawns = Map Spawns

newtype VisibleTiles = VisibleTiles (M.Map Position Tile)
instance Semigroup VisibleTiles where
  (VisibleTiles a) <> (VisibleTiles b) = VisibleTiles (M.union a b)
instance Monoid VisibleTiles where
  mempty = VisibleTiles M.empty
instance Component VisibleTiles where
  type Storage VisibleTiles = Global VisibleTiles

newtype Time = Time Int deriving (Show, Num)
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


data Direction = Horizontal | Vertical
data FoodType = Meat | Plants deriving (Eq)

totalSimulationTurns :: Int
totalSimulationTurns = 1200

grassGrowthRate :: Int
grassGrowthRate = 40
grassMaxAge :: Int
grassMaxAge = 1000
grassFoodAmount :: Int
grassFoodAmount = 50

bunnyMaxAge :: Int
bunnyMaxAge = 1000
bunnyMaxEnergy :: Int
bunnyStartingEnergy :: Int
bunnyStartingEnergy = 200
bunnyMaxEnergy = 400
bunnyFoodAmount :: Int
bunnyFoodAmount = 250
bunnySpawnRate :: Int
bunnySpawnRate = 200

type Base = (Position, Tile, Physical, TTL) --IsFood not works here?! -- report a bug on apecs
type Additional = (Growing, Moving, IsFood, Energy, Eats)
makeWorld "World" [''Position, ''Tile, ''Solid, ''Physical, ''TTL, ''Growing, ''Moving, ''Eats, ''IsFood, ''Energy, ''Spawns, ''Time, ''VisibleTiles, ''Log]

initialise :: System World ()
initialise = do
  room 0 1 40 20
  emptyFloor 1 2 39 19
  _ <- grass 15 13 0
  _ <- grass 35 3 0
  _ <- grass 35 5 0
  _ <- grass 33 3 0
  _ <- grass 33 5 0
  _ <- grass 3 15 0
  _ <- bunnySpawner 15 13
  _ <- bunnySpawner 35 5
  _ <- bunny 14 12
  _ <- bunny 30 5
  _ <- bunny 30 7
  _ <- bunny 30 6
  _ <- bunny 30 8
  wall 10 10 10 Horizontal
  wall 10 15 10 Horizontal
  wall 10 11 3  Vertical
  wall 20 11 3  Vertical
  return ()

grass
  :: ( Control.Monad.IO.Class.MonadIO m
     , Has w m Position
     , Has w m Tile
     , Has w m Growing
     , Has w m TTL
     , Has w m Physical
     , Has w m IsFood
     , Has w m EntityCounter
     )
  => Int
  -> Int
  -> Int
  -> SystemT w m Entity
grass x y p = newEntity
  ( Position (V2 x y)
  , Tile '.' 1
  , Growing grassGrowthRate p
  , TTL grassMaxAge
  , Physical
  , IsFood Plants grassFoodAmount
  )

bunny
  :: ( MonadIO m
     , Has w m Position
     , Has w m Tile
     , Has w m Physical
     , Has w m TTL
     , Has w m Moving
     , Has w m Eats
     , Has w m Energy
     , Has w m IsFood
     , Has w m EntityCounter
     )
  => Int
  -> Int
  -> SystemT w m Entity
bunny x y = newEntity
  ( Position (V2 x y)
  , Tile 'b' 5
  , Physical
  , TTL bunnyMaxAge
  , Moving
  , Eats Plants
  , Energy bunnyStartingEnergy
  , IsFood Meat bunnyFoodAmount
  )

bunnySpawner
  :: ( MonadIO m
     , Has w m Position
     , Has w m Tile
     , Has w m Spawns
     , Has w m TTL
     , Has w m EntityCounter
     )
  => Int
  -> Int
  -> SystemT w m Entity
bunnySpawner x y =
  newEntity (Position (V2 x y), Tile 'S' 2, Spawns bunnySpawnRate, TTL 800)

brick
  :: ( MonadIO m
     , Has w m Position
     , Has w m Tile
     , Has w m Solid
     , Has w m Physical
     , Has w m EntityCounter
     )
  => Int
  -> Int
  -> SystemT w m Entity
brick x y = newEntity (Position (V2 x y), Tile '#' 10, Solid, Physical)

wall
  :: ( MonadIO m
     , Has w m Position
     , Has w m Tile
     , Has w m Solid
     , Has w m Physical
     , Has w m EntityCounter
     )
  => Int
  -> Int
  -> Int
  -> Direction
  -> SystemT w m ()
wall xs ys n Horizontal =
  mapM_ (uncurry brick) [ (x, ys) | x <- [xs .. (xs + n)] ]
wall xs ys n Vertical =
  mapM_ (uncurry brick) [ (xs, y) | y <- [ys .. (ys + n)] ]

emptySpace
  :: (MonadIO m, Has w m Position, Has w m Tile, Has w m EntityCounter)
  => Int
  -> Int
  -> SystemT w m Entity
emptySpace x y = newEntity (Position (V2 x y), Tile ' ' 0)

room
  :: ( MonadIO m
     , Has w m Position
     , Has w m Tile
     , Has w m Solid
     , Has w m Physical
     , Has w m EntityCounter
     )
  => Int
  -> Int
  -> Int
  -> Int
  -> SystemT w m ()
room xmin ymin xmax ymax = mapM_
  (uncurry brick)
  (  [ (x, y) | x <- [xmin, xmax], y <- [ymin .. ymax] ]
  ++ [ (x, y) | x <- [(xmin + 1) .. (xmax - 1)], y <- [ymin, ymax] ]
  )

emptyFloor
  :: (MonadIO m, Has w m Position, Has w m Tile, Has w m EntityCounter)
  => Int
  -> Int
  -> Int
  -> Int
  -> SystemT w m ()
emptyFloor xmin ymin xmax ymax = mapM_
  (uncurry emptySpace)
  [ (x, y) | x <- [xmin .. xmax], y <- [ymin .. ymax] ]

step :: World -> System World ()
step world = do
  modify global (\(Time turns) -> Time (turns + 1))
  -- get older and die eventually
  cmap $ \(TTL turns) ->
    if turns < 0 then Right $ Not @Additional else Left $ TTL (turns - 1)
  cmap $ \(TTL turns) ->
    if turns < 0 then Right $ Not @Base else Left $ TTL (turns - 1)
  -- die if no energy
  cmap $ \(Energy e) -> if e < 0 then Right $ Not @Moving else Left $ Energy e
  cmap $ \(Energy e, Tile c z, TTL turns) -> if e < 0
    then (Energy 0, Tile '+' z, TTL 200)
    else (Energy e, Tile c z, TTL turns)
  liftIO $ runSystem grow world
  return ()

move :: System World ()
move = cmapM $ \(Moving, Position p, Energy e) -> do
  newPosition <- moveIfPossible (Position p)
  return
    ( Moving
    , newPosition
    , if Position p == newPosition then Energy e else Energy (e - 1)
    )

eat :: System World ()
eat = cmapM_ $ \(Eats wantsFood, Position p, Energy _, hungryEntity) ->
  cmapM_ $ \(IsFood hasFood foodAmount, Position p', TTL _, eatenEntity) ->
    when (p == p' && wantsFood == hasFood) $ do
      addLog ("Entity has been eaten: " ++ show eatenEntity)
      modify eatenEntity $ \(TTL _) -> TTL 0
      modify hungryEntity
        $ \(Energy e) -> Energy (min (e + foodAmount) bunnyMaxEnergy)

spawn :: System World ()
spawn = do
  Time turn <- get global
  cmapM_ $ \(Spawns t, Position (V2 x y)) -> when
    (turn `mod` t == 0)
    (do
      _ <- bunny x y
      return ()
    )


-- returns current position if move impossible
moveIfPossible :: Position -> System World Position
moveIfPossible (Position (V2 x y)) = do
  dx            <- liftIO $ randomRIO (-1, 1)
  dy            <- liftIO $ randomRIO (-1, 1)
  spaceOccupied <- cfold
    (\acc (Position (V2 px py), Solid) ->
      acc || (px == (x + dx) && py == (y + dy))
    )
    False
  if spaceOccupied
    then return (Position (V2 x y))
    else return (Position (V2 (x + dx) (y + dy)))

getGrassPositionIfPossible :: Position -> System World Position
getGrassPositionIfPossible (Position (V2 x y)) = do
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
      Position (V2 x' y') <- getGrassPositionIfPossible (Position p)
      randomPeriod        <- liftIO $ randomRIO (0, grassGrowthRate)
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
  entities  <- cfold (\(acc :: Int) (Tile _ _) -> acc + 1) 0
  addLog ("Current turn: " ++ show turn ++ " Entities: " ++ show entities)
  Log logs <- get global
  liftIO (setCursor 0 23)
  mapM_ (liftIO . onTerminal . drawString) (take 1 logs)

setCursor :: (MonadIO m, Control.Monad.Catch.MonadMask m) => Int -> Int -> m ()
setCursor x y = onTerminal $ T.setCursorPosition $ T.Position y x

drawChar :: T.MonadScreen m => Char -> Int -> Int -> m ()
drawChar c x y = do
  T.setCursorPosition $ T.Position y x
  T.putChar c

drawString :: String -> T.TerminalT LocalTerminal IO ()
drawString = T.putStringLn

loop :: World -> Int -> IO ()
loop world turns = do
  runSystem (step world)     world
  runSystem spawn            world
  runSystem move             world
  runSystem eat              world
  runSystem updateVisibleMap world
  runSystem logging          world
  onTerminal T.flush

  when (turns > 0) $ loop world (turns - 1)

drawCharOnTerminal :: (MonadIO m, MonadMask m) => Char -> Int -> Int -> m ()
drawCharOnTerminal c x y = onTerminal (drawChar c x y)

onTerminal :: (MonadIO m, MonadMask m) => T.TerminalT LocalTerminal m a -> m a
onTerminal f = T.withTerminal $ T.runTerminalT f


main :: IO ()
main = do
  world <- initWorld
  onTerminal T.hideCursor
  onTerminal $ T.eraseInDisplay T.EraseAll
  runSystem initialise world
  loop world totalSimulationTurns
  drawCharOnTerminal 'E' 1 22
