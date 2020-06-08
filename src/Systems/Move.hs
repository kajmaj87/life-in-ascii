module Systems.Move
  ( move
  , quadrance
  , findClosestPoint
  )
where

import           Components
import           Apecs
import           Linear                         ( V2(..) )
import           System.Random

quadrance :: V2 Int -> V2 Int -> Int
quadrance (V2 x y) (V2 x' y') =
  (x - x') ^ (2 :: Integer) + (y - y') ^ (2 :: Integer)

findClosestPoint :: V2 Int -> [V2 Int] -> Maybe (V2 Int)
findClosestPoint pos (p : ps) = Just
  (foldr (\a b -> if quadrance' a < quadrance' b then a else b) p ps)
  where quadrance' = quadrance pos
findClosestPoint _ _ = Nothing

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

move :: System World ()
move = cmapM $ \(Action Move, Moving, Goal _, Position p, Energy e) -> do
  newPosition <- moveIfPossible (Position p)
  return
    ( newPosition
    , if Position p == newPosition then Energy e else Energy (e - 1)
    )
