{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell            #-}

module Components where
import           Apecs
import           Apecs.Experimental.Reactive
import qualified Data.Map                      as M
import           Data.Array
import           Linear                         ( V2(..) )

--- Global components
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

--- Local components
gameMaxX :: Int
gameMaxX = 40
gameMaxY :: Int
gameMaxY = 20
instance Bounded Position where
  minBound = Position (V2 0 0)
  maxBound = Position (V2 gameMaxX gameMaxY)
newtype Position = Position (V2 Int) deriving (Show, Ord, Eq)
  deriving (Ix) via (V2 Int)
instance Component Position where
  type Storage Position = Reactive (IxMap Position) (Apecs.Map Position)

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

data Brain = Brain
instance Component Brain where
  type Storage Brain = Map Brain

newtype Goal = Goal GoalType
instance Component Goal where
  type Storage Goal = Map Goal

newtype Action = Action ActionType
instance Component Action where
  type Storage Action = Map Action

--- Other types
data Direction = Horizontal | Vertical
data FoodType = Meat | Plants deriving (Eq)
data GoalType = Eat
data ActionType = Move

--- World creation and type groups for deleting entities
type All
  = ( Position
    , Tile
    , Physical
    , TTL
    , (Growing, Moving, IsFood, Energy, Eats, Brain, Goal, Action)
    )

makeWorld "World" [''Position, ''Tile, ''Solid, ''Physical, ''TTL, ''Growing, ''Moving, ''Eats, ''IsFood, ''Energy, ''Spawns, ''Brain, ''Goal, ''Action, ''Time, ''VisibleTiles, ''Log]
