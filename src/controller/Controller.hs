{-# LANGUAGE NamedFieldPuns #-}
module Controller.Controller where

import Model.GameState
import Model.Player (movePlayer)
import Model.Shooting
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

step :: Float -> GameState -> IO GameState
step elapsedTime gs = return (updateOnStep elapsedTime gs)

input :: Event -> GameState -> IO GameState
input _ = return

-- This function takes an event, a certain keyboard input, and a current gamestate, and returns the new gamestate.
keyboardInputHandler :: Event -> GameState -> GameState
keyboardInputHandler (EventKey pressedButton _ _ _) gameState  = case phase gamestate of
  -- If the gamephase is 'playing', the following input is considered:
  Playing -> case pressedButton of
    SpecialKey KeyUp -> gameState {player =  movePlayer (player gameState) verticalMovementStep} -- Move the player with the upwards.
    SpecialKey KeyDown -> gameState {player = movePlayer (player gameState) ((-1) * verticalMovementStep)} -- Move the player with the downwards.
    Char r -> initalState -- Reset the gamestate to the initalGamestate by pressing r.
    Char s -> gameState {bullets = bullets gameState ++ shoot (player gameState)} -- Shoot when 's' is pressed, update the current bullet list of the gamestate with the new bullet(s).
    Char p -> gameState {phase =  Paused} -- Pause the game.
  -- When the game is paused, you can only reset the game, unpause or save the gamestate to JSON.
  Paused -> case pressedButton of
    Char r -> initalState
    Char p -> gameState {phase =  Playing} -- Unpause the game.
    -- Char q -> -- Save gameState to JSON.
  -- When game over, you can only reset the game.
  GameOver -> case pressedButton of
    Char r -> initalState

-- Nothing happens if any other type of keyboard event is triggered.
keyboardInputHandler _ gameState = gameState


verticalMovementStep = 5.0 -- Amount of vertical movement of player based on input.