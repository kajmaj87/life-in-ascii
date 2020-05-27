module Main where
import           System.IO                      ( stdin
                                                , hSetEcho
                                                , hSetBuffering
                                                , BufferMode(..)
                                                )

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  draw $ e $ initGame ()



draw :: [Entity] -> IO ()
draw = mapM_ drawEntity

drawEntity :: Entity -> IO ()
drawEntity entity = case render entity of
  Just (Render (x, y) c) -> drawChar x y c
  _                      -> return ()

drawChar :: Int -> Int -> Char -> IO ()
drawChar x y c = case maybeMove x y of
  ""         -> return ()
  escapeCode -> putStr $ escapeCode ++ [c]

maybeMove :: Int -> Int -> String
maybeMove x y
  | y > 0
  , x > 0
  = let escapeCode = "[" ++ show y ++ ";" ++ show x ++ "f"
    in  "\ESC" ++ escapeCode
  | otherwise
  = ""

data Game = Game { e :: [Entity] }
data Entity = Entity { components :: [Component] }

type Position = (Int, Int)
data Component = Render Position Char | Alive | Food FoodType
data FoodType = Meat | Plant

render :: Entity -> Maybe Component
render entity =
  let findRender (x : xs) = case x of
        Render _ _ -> Just x
        _          -> findRender xs
      findRender [] = Nothing
  in  findRender $ components entity



floorTile :: Int -> Int -> Entity
floorTile x y = Entity { components = [Render (x, y) '.'] }

meatTile :: Int -> Int -> Entity
meatTile x y = Entity { components = [Food Meat, Render (x, y) '%'] }

plantTile :: Int -> Int -> Entity
plantTile x y = Entity { components = [Food Plant, Render (x, y) '&'] }

animal :: Int -> Int -> Entity
animal x y = Entity { components = [Food Meat, Render (x, y) '*'] }

generateLevel :: Int -> Int -> [Entity]
generateLevel xmax ymax =
  [ floorTile x y | x <- [1 .. xmax], y <- [1 .. ymax] ]
    ++ [meatTile 10 10, meatTile 15 5, plantTile 5 15, plantTile 18 20]

initGame :: () -> Game
initGame _ = Game { e = generateLevel 40 20 }
