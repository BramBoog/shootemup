module Controller.Controller where
import Graphics.Gloss.Interface.IO.Game
import Model.GameState
import Model.Player

-- This function takes an event, a certain keyboard input, and a current gamestate, and returns the new gamestate.
keyboardInputHandler :: Event -> GameState -> GameState
-- Move the player with the up and down arrow keys
keyboardInputHandler (EventKey (SpecialKey KeyUp) Up _ _) gameState = gameState {player = moveUp player}
keyboardInputHandler (EventKey (SpecialKey KeyUp) Down _ _) gameState = gameState {player = moveDown player}
    where
        -- Update the position of the player
        moveUp player = movePlayer player verticalMovementStep
        moveDown player = movePlayer player -verticalMovementStep
        verticalMovementStep = 5
