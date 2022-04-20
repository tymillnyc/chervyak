module Main where
import DrawApp
import UpdateApp
import Shared
import HandleEvent
import InitState
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Lib


main :: IO ()
main = do
	conn <- getConnection
	rec <- getTop10 conn
	playIO window grassColor fps (initState conn rec) drawApp handleEvent updateApp