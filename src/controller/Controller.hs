module Controller.Controller where

import Model.GameState
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

step :: Float -> GameState -> IO GameState
step elapsedTime gs = return (updateOnStep elapsedTime gs)

input :: Event -> GameState -> IO GameState
input _ = return

-- This function takes an event, a certain keyboard input, and a current gamestate, and returns the new gamestate.
-- keyboardInputHandler :: Event -> GameState -> GameState
-- keyboardInputHandler (EventKey (SpecialKey KeyUp))