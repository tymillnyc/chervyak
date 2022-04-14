module HandleEvent where 
import Shared
import InitState
import Graphics.Gloss.Interface.Pure.Game

-- handle keys
handleEvent :: Event -> AppState -> AppState
handleEvent event state = 
  case screen state of
    NameField -> handleText event state
    Menu -> handleMenu event state
    Table -> handleTable event state
    Game -> handleMotion event state

handleText :: Event -> AppState -> AppState
handleText (EventKey (SpecialKey keyButton) Down _ _) state = 
  case keyButton of
    KeyEnter -> if ((length(name state)) /= 0) then state {screen = Menu} else state
    _ -> state
handleText (EventKey (Char key) Down _ _) state = 
    case key of
        '\b' -> state {name = deleteLast (name state) }
        letter -> if (length (name state) < 28) then 
                    state {name = (name state) ++ [letter]} 
                  else 
                    state
    
handleText _ state = state

deleteLast :: [a] -> [a]
deleteLast []     = []
deleteLast [_]    = []
deleteLast (h : t)  =[h] ++ deleteLast t

handleMenu :: Event -> AppState -> AppState
handleMenu (EventKey (SpecialKey keyButton) Down _ _) state = 
  case keyButton of
      KeyEnter -> case (selected state) of
                    0 -> state {screen = Game}
                    1 -> state {screen = Table}
                    2 -> state {screen = NameField}
                    _ -> state
      KeyDown -> if ((selected state) /= 2) then state {selected = (selected state) + 1} else state
      KeyUp -> if ((selected state) /= 0) then state {selected = (selected state) - 1} else state
      _ -> state
handleMenu _ state = state

handleTable :: Event -> AppState -> AppState
handleTable (EventKey (SpecialKey keyButton) Down _ _) state = 
  case keyButton of
    KeyEnter -> state {screen = Menu}
    _ -> state
handleTable _ state = state

handleMotion :: Event -> AppState -> AppState
handleMotion (EventKey (SpecialKey keyButton) Down _ _) state = 
  case keyButton of
  KeyLeft -> changeDirection (direction state) CrawlLeft state
  KeyRight -> changeDirection (direction state) CrawlRight state
  KeyUp -> changeDirection (direction state) CrawlUp state
  KeyDown -> changeDirection (direction state) CrawlDown state
  KeySpace -> if isGameOver state 
     then (initState {screen = Menu, name = (name state)})
     else state
  _ -> state
handleMotion _ state = state

changeDirection :: Direction -> Direction -> AppState -> AppState
changeDirection CrawlRight CrawlLeft state = state{direction = CrawlRight}
changeDirection CrawlDown CrawlUp state = state{direction = CrawlDown}
changeDirection CrawlLeft CrawlRight state = state{direction = CrawlLeft}
changeDirection CrawlUp CrawlDown state = state{direction = CrawlUp}
changeDirection _ newDirection state = state{direction = newDirection}
  