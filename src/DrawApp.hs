module DrawApp where 
import Shared
import Graphics.Gloss

drawApp :: AppState -> Picture
drawApp state = pictures
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
          translate (-250) (-100) $ 
          scale 0.3 0.3 $ 
          text "Press SPACE to try again"
        ]
    else pictures []