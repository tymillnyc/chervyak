module Main where
import Lib
import DrawApp
import UpdateApp
import Shared
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
-- initial structure
-- snake spawns in the center and has a head and one tail cell
initState :: AppState
initState = AppState {
  snakeCoordinates = [(dx, dy), (dx - 1, dy)]
, foodCoordinates = (dx + 1, dy - 2)
, direction = CrawlRight
, generator = mkStdGen seed
, isGameOver = False
} where dx = columns `div` 2
        dy = rows `div` 2
 
-- handle keys
handleEvent :: Event -> AppState -> AppState
handleEvent (EventKey (SpecialKey keyButton) Down _ _) state = 
  case keyButton of
  KeyLeft -> state {direction = CrawlLeft}
  KeyRight -> state {direction = CrawlRight}
  KeyUp -> state {direction = CrawlUp}
  KeyDown -> state {direction = CrawlDown}
  KeySpace -> if isGameOver state 
     then initState
     else state

  
handleEvent _ state = state
 
main :: IO ()
main = play window grassColor fps initState drawApp handleEvent updateApp