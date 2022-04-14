module InitState where
import Shared
import System.Random

-- initial structure
-- snake spawns in the center and has a head and one tail cell
-- name is empty
-- visible is true; then will change between true and false every fps
-- selected menu field is the top one
-- fake table of records
initState :: AppState
initState = AppState {
  snakeCoordinates = [(dx, dy), (dx - 1, dy)]
, foodCoordinates = (dx + 1, dy - 2)
, direction = CrawlRight
, generator = mkStdGen seed
, isGameOver = False
, screen = NameField

, name = ""
, visible = True

, selected = 0

, records = [("Christie", 100), 
             ("Alexander", 100), 
             ("player1", 80), 
             ("player2", 70), 
             ("player3", 65), 
             ("player4", 45), 
             ("player5", 40), 
             ("player6", 35), 
             ("player7", 30), 
             ("player8", 25)]
} where dx = columns `div` 2
        dy = rows `div` 2