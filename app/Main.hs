module Main where

import Controller.Controller
import Controller.FileSystem
import View.View
import View.Window
import Model.GameState (initialState, updateOnStep)

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
                      updateOnStep
