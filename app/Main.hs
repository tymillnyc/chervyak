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
handleEvent (EventKey (SpecialKey KeyLeft ) Down _ _) state = 
  state {direction = CrawlLeft}
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) state = 
  state {direction = CrawlRight}
handleEvent (EventKey (SpecialKey KeyUp   ) Down _ _) state = 
  state {direction = CrawlUp}
handleEvent (EventKey (SpecialKey KeyDown ) Down _ _) state = 
  state {direction = CrawlDown}
handleEvent (EventKey (SpecialKey KeySpace ) Down _ _) state = if isGameOver state 
                                                                  then initState
                                                                  else state
  
handleEvent _ state = state
 
main :: IO ()
main = play window grassColor fps initState drawApp handleEvent updateApp