module UpdateApp where 
import Shared
import System.Random

-- decide which function to use based on current screen
updateApp :: Float -> AppState -> IO AppState
updateApp upd state = 
  return $ case screen state of
    NameField -> updateTextField upd state
    Menu -> state -- don't need to update anything
    Table -> state -- don't need to update anything
    Game -> (updateGame upd state)
 
-- flickering
updateTextField :: Float -> AppState -> AppState
updateTextField _ appState = appState{visible = not (visible appState)}

-- snake movement and analyzing if snake is dead; also generating food
updateGame :: Float -> AppState -> AppState
updateGame _ appState
  | isGameOver appState = appState
  | inGrass (dx, dy) && not (isSnake (dx, dy) (snakeCoordinates appState))
    = if isFood (dx, dy) (foodCoordinates appState)
      then generateNewFood appState
        {snakeCoordinates = (dx, dy) : oldSnakeCoordinates}
      else appState {snakeCoordinates = (dx, dy) : newSnakeCoordinates}
  | otherwise = appState {isGameOver = True}
  where
      oldSnakeCoordinates = snakeCoordinates appState
      newSnakeCoordinates = init oldSnakeCoordinates
      (x, y) = head newSnakeCoordinates
      (dx, dy)
        = case direction appState of
                CrawlUp -> (x, y + 1)
                CrawlRight -> (x + 1, y)
                CrawlDown -> (x, y - 1)
                CrawlLeft -> (x - 1, y)

-- generating food at random coordinates that are not snake coordinates     
generateNewFood :: AppState -> AppState
generateNewFood appState = if isSnake (dx, dy) (snakeCoordinates appState) 
                           then generateNewFood appState 
                           else appState {foodCoordinates = (dx, dy), generator = newGenerator2}
  where (dx, newGenerator) = randomR (1, columns) (generator appState)
        (dy, newGenerator2) = randomR (1, rows) newGenerator
 
 
-- check if cell is in game field
inGrass :: (Int, Int) -> Bool
inGrass (x, y) = x /= 0 && y /= 0 && x /= columns && y /= rows
 
-- check if cell is a part of snake
isSnake :: (Int, Int) -> Snake -> Bool
isSnake (x, y) = elem (x, y)
 
-- check if cell is a food cell
isFood :: (Int, Int) -> Food -> Bool
isFood (x, y) food = (x, y) == food