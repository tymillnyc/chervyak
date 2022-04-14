module Main where
import DrawApp
import UpdateApp
import Shared
import HandleEvent
import InitState
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

 
main :: IO ()
main = play window grassColor fps initState drawApp handleEvent updateApp