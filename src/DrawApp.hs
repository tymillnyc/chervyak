module DrawApp where 
import Shared
import Graphics.Gloss

drawApp :: AppState -> Picture
drawApp state = 
  case screen state of
    NameField -> drawNameField state
    Menu -> drawMenu state
    Table -> drawTable state
    Game -> drawGame state
  
drawNameField :: AppState -> Picture
drawNameField state = pictures 
    [ drawBorders
    , drawTextField state
    ]

drawTextField :: AppState -> Picture
drawTextField state = pictures
        [ color foodColor $ 
          translate (-200) 0 $
          scale 0.35 0.35 $
          text "Enter your name"
       ,  color (if (visible state) then blue else grassColor) $ 
          translate (-250) (-100) $ 
          scale 0.3 0.3 $ 
          text ((name state) ++ "|")
       ,  color blue $ 
          translate (-250) (-100) $ 
          scale 0.3 0.3 $ 
          text (name state)
       ,  color foodColor $ 
          translate (-250) (-200) $ 
          scale 0.3 0.3 $ 
          text (
            if (length (name state) /= 0) then 
              "Press ENTER to continue" 
            else 
              ""
          )
        ]
 
drawMenu :: AppState -> Picture
drawMenu state = pictures
    [ drawBorders
    , drawMenuElements state
    ]

drawMenuElements :: AppState -> Picture
drawMenuElements state = pictures
      [ color wormColor $ 
        translate (-115) 200 $
        scale 0.4 0.4 $
        text "Chervyak"
      , color (if ((selected state) == 0) then foodColor else blue) $ 
        translate (-50) (50) $ 
        scale 0.3 0.3 $ 
        text "Play"
      , color (if ((selected state) == 1) then foodColor else blue) $ 
        translate (-75) (-50) $ 
        scale 0.3 0.3 $ 
        text "Top 10"
      , color (if ((selected state) == 2) then foodColor else blue) $ 
        translate (-175) (-150) $ 
        scale 0.3 0.3 $ 
        text "Change your name"
      , color wormColor $ 
        translate (- (fromIntegral (length (name state)) * 8)) (-300) $ 
        scale 0.2 0.2 $ 
        text (name state)
        ]

drawTable :: AppState -> Picture
drawTable state = pictures 
    [ drawBorders
    , drawTableElements state
    ]

drawTableElements :: AppState -> Picture
drawTableElements state = pictures(
          ([
            color wormColor $ 
            translate (-215) 250 $
            scale 0.4 0.4 $
            text "Top 10 players"
          , color foodColor $ 
            translate (-50) (-300) $
            scale 0.3 0.3 $
            text "exit"
          ]) ++ (tableTiles (records state) 50))
        

tableTiles :: Records -> Float -> [Picture]
tableTiles [] _ = []
tableTiles (x:xs) offset = 
        ((color blue $ 
        translate (-100) (250 - offset) $
        scale 0.2 0.2 $
        text ((fst x) ++ " " ++ show (snd x))) : (tableTiles xs (offset + 50.0)))

drawGame :: AppState -> Picture
drawGame state = pictures
    [ drawBorders
    , drawFood state
    , drawSnake state
    , drawGameOver state
    ]

drawBorders :: Picture
drawBorders  = pictures 
              [ fillBorder (fromIntegral columns / 2, 0) size1
              , fillBorder (fromIntegral columns / 2, fromIntegral rows) size1
              , fillBorder (0, fromIntegral rows / 2) size2
              , fillBorder (fromIntegral columns, fromIntegral rows / 2) size2 
              ]
              where size1 = (fromIntegral windowWidth + windowScale, windowScale)
                    size2 = (windowScale, fromIntegral windowHeight)
 
fillBorder :: (Float, Float) -> (Float, Float) -> Picture
fillBorder (dx, dy) (width, height) = color borderColor $
                             translate x y $ 
                              rectangleSolid width height
                              where x = (dx * windowScale - fromIntegral windowWidth / 2)
                                    y = (dy * windowScale - fromIntegral windowHeight / 2)
 
drawFood :: AppState -> Picture
drawFood state = drawCell foodColor (foodCoordinates state)
 
drawSnake :: AppState -> Picture
drawSnake state = case snakeCoordinates state of
        (x : xs) -> pictures (drawCell wormHeadColor x : map (drawCell wormColor) xs)
        _ -> pictures []
 
drawCell :: Color -> (Int, Int) -> Picture
drawCell c (x, y) =
    color c $ translate dx dy (rectangleSolid windowScale windowScale)
    where dx = windowScale * fromIntegral x - (fromIntegral windowWidth / 2)
          dy = windowScale * fromIntegral y  - (fromIntegral windowHeight / 2)
 
drawGameOver :: AppState -> Picture
drawGameOver state = if isGameOver state
    then pictures
        [ color backColor $
          scale 1.5 1.5 $
          rectangleSolid (fromIntegral windowHeight) (fromIntegral windowWidth)
        , color foodColor $ 
          translate (-200) 0 $
          scale 0.5 0.5 $
          text "GAME OVER"
        , color foodColor $
          translate (-100) (-50) $ 
          scale 0.4 0.4 $
          text $ "score: " ++ show ((length(snakeCoordinates state) - 2) * 5)
       ,  color blue $ 
          translate (-300) (-100) $ 
          scale 0.3 0.3 $ 
          text "Press SPACE to go to menu"
        ]
    else pictures []