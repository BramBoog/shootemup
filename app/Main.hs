module Main where

import Controller.Controller
import Controller.FileSystem
import View.View
import Model.GameState (initialState)

import Graphics.Gloss.Interface.IO.Game
import System.Environment

main :: IO ()
main = do args <- getArgs
          initState <- case args of
            [] -> return initialState
            "LoadGame":_ -> loadGameState
            _ -> error "Unrecognised command line argument"

          playIO window
                      black
                      10
                      initState
                      view
                      keyboardInputHandler
                      step
