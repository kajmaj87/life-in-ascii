--- Imports/Language settings
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}


module Main
  ( main
  )
where

import           Systems.Move
import           Components

import           Apecs
import           Apecs.Experimental.Reactive
import           Linear                         ( V2(..) )
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Catch
import qualified System.Terminal               as T
import           System.Terminal.Internal
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           System.Random

--- Constants

totalSimulationTurns :: Int
totalSimulationTurns = 200

grassGrowthRate :: Int
grassGrowthRate = 40
grassMaxAge :: Int
grassMaxAge = 1000
grassFoodAmount :: Int
grassFoodAmount = 50

bunnyMaxAge :: Int
bunnyMaxAge = 1000
bunnyMaxEnergy :: Int
bunnyMaxEnergy = 400
bunnyStartingEnergy :: Int
bunnyStartingEnergy = 200
bunnyFoodAmount :: Int
bunnyFoodAmount = 250
bunnySpawnRate :: Int
bunnySpawnRate = 200

--- Entity templates
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
     , Has w m Brain
     , Has w m Eats
     , Has w m Goal
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
  , (Brain, Goal Eat, Eats Plants, Energy bunnyStartingEnergy)
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

--- Terminal functions
setCursor :: (MonadIO m, Control.Monad.Catch.MonadMask m) => Int -> Int -> m ()
setCursor x y = onTerminal $ T.setCursorPosition $ T.Position y x

drawChar :: T.MonadScreen m => Char -> Int -> Int -> m ()
drawChar c x y = do
  T.setCursorPosition $ T.Position y x
  T.putChar c

drawString :: String -> T.TerminalT LocalTerminal IO ()
drawString = T.putStringLn

drawCharOnTerminal :: (MonadIO m, MonadMask m) => Char -> Int -> Int -> m ()
drawCharOnTerminal c x y = onTerminal (drawChar c x y)

onTerminal :: (MonadIO m, MonadMask m) => T.TerminalT LocalTerminal m a -> m a
onTerminal f = T.withTerminal $ T.runTerminalT f

--- Systems
initialise :: System World ()
initialise = do
  room 0 1 gameMaxX gameMaxY
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

step :: World -> System World ()
step world = do
  modify global (\(Time turns) -> Time (turns + 1))
  -- get older and die eventually
  cmap $ \(TTL turns) ->
    if turns < 0 then Right $ Not @All else Left $ TTL (turns - 1)
  cmap $ \(TTL turns) ->
    if turns < 0 then Right $ Not @All else Left $ TTL (turns - 1)
  -- die if no energy
  cmap $ \(Energy e) -> if e < 0 then Right $ Not @Moving else Left $ Energy e
  cmap $ \(Energy e, Tile c z, TTL turns) -> if e < 0
    then (Energy 0, Tile '+' z, TTL 200)
    else (Energy e, Tile c z, TTL turns)
  liftIO $ runSystem grow world
  return ()

--findNearestFoodInRange
--  :: FoodType -> Int -> Position -> System World (Maybe Position)
--findNearestFoodInRange t r (Position (V2 x y)) =
--  let distance a b = (a - x)^2 + (b - y)^2
--      eligableCoords =
--          [ (V2 x' y')
--          | x' <- [x - r .. x + r]
--          , y' <- [y - r .. y + r]
--          , distance x' y' <= r ^ 2
--          ]
--      positionHasFood   = positionHasAny @IsFood
--      positionsWithFood = filter (\(p, (System World hasFood)) -> hasFood) . map (\p -> (p, positionHasFood p)) eligableCoords
--  in  foldl' (\bestPoint p -> if uncurry distance bestPoint > uncurry distance p then uncurry distance p else uncurry distance bestPoint) (head positionsWithFood) 



eat :: System World ()
-- TODO Change double loop to entitiesAtPosition and hasAny
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

think :: System World ()
think = cmapM_ $ \(Brain, Moving, Goal Eat, entity) -> do
  set entity (Action Move)
  return ()

-- returns current position if move impossible

entitiesAtPosition :: V2 Int -> System World [Entity]
entitiesAtPosition c = withReactive $ ixLookup (Position c)

hasAny :: forall c . (Get World IO c) => [Entity] -> System World Bool
hasAny = fmap (not . null) . filterM (`exists` Proxy @c)

--positionHasAny :: forall c . (Get World IO c) => V2 Int -> System World Bool
--positionHasAny = entitiesAtPosition =>> hasAny

getRandomNearbyPosition :: Position -> System World Position
getRandomNearbyPosition (Position (V2 x y)) = do
  dx <- liftIO $ randomRIO (-1, 1)
  dy <- liftIO $ randomRIO (-1, 1)
  return (Position (V2 (x + dx) (y + dy)))

grow :: System World ()
grow = do
  -- grow 
  cmap $ \(Growing p current) -> Growing p (current + 1)
  possibleGrowthPositions <- cfold
    (\acc (Position p, Growing period current) ->
      if current `mod` period == 0 then S.insert (Position p) acc else acc
    )
    mempty
  mapM_
    (\(Position p) -> do
      Position (V2 x y) <- getRandomNearbyPosition (Position p)
      entities          <- entitiesAtPosition (V2 x y)
      isGrowthForbidden <- hasAny @Physical entities
      unless
        isGrowthForbidden
        (do
          randomPeriod <- liftIO $ randomRIO (0, grassGrowthRate)
          _            <- grass x y randomPeriod
          return ()
        )
    )
    possibleGrowthPositions


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
  entities  <- cfold (\(acc :: Int) (Growing _ _) -> acc + 1) 0
  addLog ("Current turn: " ++ show turn ++ " Entities: " ++ show entities)
  Log logs <- get global
  liftIO (setCursor 0 23)
  mapM_ (liftIO . onTerminal . drawString) (take 1 logs)

--- Main Loop
loop :: World -> Int -> IO ()
loop world turns = do
  runSystem (step world)     world
  runSystem spawn            world
  runSystem think            world
  runSystem move             world
  runSystem eat              world
  runSystem updateVisibleMap world
  runSystem logging          world
  onTerminal T.flush

  when (turns > 0) $ loop world (turns - 1)

main :: IO ()
main = do
  world <- initWorld
  onTerminal T.hideCursor
  onTerminal $ T.eraseInDisplay T.EraseAll
  runSystem initialise world
  loop world totalSimulationTurns
  drawCharOnTerminal 'E' 1 22
