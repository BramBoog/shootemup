{-# LANGUAGE NamedFieldPuns #-}
module Controller.Controller where
import Graphics.Gloss.Interface.IO.Game
import Model.GameState
import Model.Player (movePlayer)
import Model.Shooting

-- This function takes an event, a certain keyboard input, and a current gamestate, and returns the new gamestate.
keyboardInputHandler :: Event -> GameState -> GameState

keyboardInputHandler (EventKey pressedButton _ _ _) gameState  = case pressedButton of
  Char r -> initalGameState -- Reset the gamestate to the initalGamestate by pressing r, variable does not exist yet.
  Char s -> gameState {bullets = bullets gameState ++ shoot (player gameState)} -- Shoot when 's' is pressed, update the current bullet list of the gamestate with the new bullet(s).
  Char p -> gameState {isPaused = not (isPaused gameState)} -- Pause the game, or unpause if the game is already paused.
  -- Char q -> if isPaused gameState = -- Save gameState to JSON -- else Nothing

  -- Move the player with the up and down arrow keys
  SpecialKey KeyUp -> gameState {player =  movePlayer (player gameState) verticalMovementStep}
  SpecialKey KeyDown -> gameState {player = movePlayer (player gameState) ((-1) * verticalMovementStep)}

  -- Nothing happens if any other key is pressed.
  _ -> gameState

-- Nothing happens if any other type of keyboard event is triggered.
keyboardInputHandler _ gameState = gameState


verticalMovementStep = 5.0