module InitState where
import Shared
import System.Random
import Database.HDBC.Sqlite3 (connectSqlite3, Connection())

-- initial structure
-- snake spawns in the center and has a head and one tail cell
-- name is empty
-- visible is true; then will change between true and false every fps
-- selected menu field is the top one
-- fake table of records
initState ::Connection -> Records -> AppState
initState conn rec = AppState {
  snakeCoordinates = [(dx, dy), (dx - 1, dy)]
, foodCoordinates = (dx + 1, dy - 2)
, direction = CrawlRight
, generator = mkStdGen seed
, isGameOver = False
, screen = NameField
, name = ""
, visible = True
, selected = 0
, records = rec
, connection = conn
, snakeId = -1
} where dx = columns `div` 2
        dy = rows `div` 2