module Controller.Controller where

-- This function takes an event, a certain keyboard input, and a current gamestate, and returns the new gamestate.
keyboardInputHandler :: Event -> GameState -> GameState
keyboardInputHandler (EventKey (SpecialKey KeyUp)