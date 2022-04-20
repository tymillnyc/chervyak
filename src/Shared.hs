module Shared where 
import Graphics.Gloss
import System.Random
import Database.HDBC.Sqlite3 (connectSqlite3, Connection())
import Lib
-- InWindow: launch the app in window mode
-- windowName: defines window name
-- windowSize: defines window size
-- windowPosition: defines position of the window
window :: Display
window = InWindow windowName windowSize windowPosition

windowName :: String
windowName = "Chervyak"

windowWidth :: Int
windowWidth = 640

windowHeight :: Int
windowHeight = 640

windowSize :: (Int, Int)
windowSize = (windowWidth, windowHeight)

windowPosition :: (Int, Int)
windowPosition = (0, 0)

-- Colors used in a game
grassColor :: Color
grassColor = makeColorI 63 64 30 255

wormColor :: Color
wormColor = makeColorI 60 21 24 255

wormHeadColor :: Color
wormHeadColor = makeColorI 90 31 34 255

foodColor :: Color
foodColor = makeColorI 242 43 41 255

borderColor :: Color 
borderColor = makeColorI 119 194 46 255

backColor :: Color 
backColor = makeColorI 0 0 0 75

-- FPS of the game
fps :: Int
fps = 10

-- scale of grid
windowScale :: Float
windowScale = 20

-- seed for random
seed :: Int
seed = 19

-- columns and rows of grid
columns :: Int
columns = fst windowSize `div` round windowScale
rows :: Int
rows = snd windowSize `div` round windowScale

-- Directions of the game
data Direction = CrawlUp | CrawlDown | CrawlLeft | CrawlRight deriving Eq

-- Game screens
data Screen = NameField | Menu | Table | Game deriving Eq


-- where food located on the grid
type Food = (Int, Int)
-- where all parts of snake located on the grid
type Snake = [(Int, Int)]

-- one record --name-score--
type Record = (String, Int)
-- table of records
type Records = [Record]

-- main structure
-- snakeCoordinates - coordinates of every part of snake
-- foodCoordinates - coordinates of food
-- direction - direction where snake goes right now
-- generator - generator for random food generation
-- isGameOver - a flag that indicates whether the game is ovet not
-- screen - current game screen
-- name - player name
-- visible - variable for text field indicator(for it to flicker)
-- selected - selected menu tile
-- records - table of records
data AppState = AppState {
  snakeCoordinates :: Snake
, foodCoordinates :: Food
, direction :: Direction
, generator :: StdGen
, isGameOver :: Bool
, screen :: Screen
, name :: String
, visible :: Bool
, selected :: Int
, records :: Records
, connection :: Connection
, snakeId :: ID
}
