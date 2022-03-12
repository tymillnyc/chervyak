module Main where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random

-- InWindow: launch the app in window mode
-- windowName: defines window name
-- windowSize: defines window size
-- windowPosition: defines position of the window
window :: Display
window = InWindow windowName windowSize windowPosition

windowName :: String
windowName = "Червяк"

windowSize :: (Int, Int)
windowSize = (640, 480)

windowPosition :: (Int, Int)
windowPosition = (100, 100)

-- Colors used in a game
grassColor :: Color
grassColor = makeColorI 111 115 2 1

wormColor :: Color
wormColor = makeColorI 89 59 2 1

foodColor :: Color
foodColor = makeColorI 217 61 4 1

-- FPS of the game
fps :: Int
fps = 10

-- scale of grid
windowScale :: Int
windowScale = 15

-- seed for random
seed :: Int
seed = 19

-- columns and rows of grid
columns :: Int
columns = fst windowSize `div` windowScale
rows :: Int
rows = snd windowSize `div` windowScale

-- Directions of the game
data Direction = CrawlUp | CrawlDown | CrawlLeft | CrawlRight

-- where food located on the grid
type Food = (Int, Int)
-- where all parts of snake located on the grid
type Snake = [(Int, Int)]

-- main structure
-- snakeCoordinates - coordinates of every part of snake
-- foodCoordinates - coordinates of food
-- direction - direction where snake goes right now
-- generator - generator for random food generation
-- isGameOver - a flag that indicates whether the game is ovet not
data AppState = AppState {
  snakeCoordinates :: Snake
, foodCoordinates :: Food
, direction :: Direction
, generator :: StdGen
, isGameOver :: Bool
}

-- initial structure
-- snake spawns in the center and has a head and one tail block
initState :: AppState
initState = AppState {
  snakeCoordinates = [(dx, dy), (dx - 1, dy)]
, foodCoordinates = (dx + 1, dy - 2)
, direction = CrawlRight
, generator = newStdGen seed
, isGameOver = False
} where dx = columns `div` 2
        dy = rows `div` 2

-- handle keys
handleEvent :: Event -> AppState -> AppState
handleEvent (EventKey (SpecialKey KeyLeft ) Down _ _) state = state {direction = CrawlLeft}
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) state = state {direction = CrawlRight} 
handleEvent (EventKey (SpecialKey KeyUp   ) Down _ _) state = state {direction = CrawlUp}  
handleEvent (EventKey (SpecialKey KeyDown ) Down _ _) state = state {direction = CrawlDown}  
handleEvent _ state = state

main :: IO ()
main = play window grassColor fps initState drawApp handleEvent updateApp
