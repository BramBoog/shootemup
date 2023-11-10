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
input event gamestate = return (keyboardInputHandler event gamestate)

-- This function takes an event, a certain keyboard input, and a current gamestate, and returns the new gamestate.
keyboardInputHandler :: Event -> GameState -> GameState
keyboardInputHandler (EventKey pressedButton _ _ _) gameState@GameState{player, bullets}  = case phase gameState of
  -- If the gamephase is 'playing', the following input is considered:
  Playing -> case pressedButton of
    SpecialKey KeyUp -> gameState {player =  movePlayer player verticalMovementStep} -- Move the player upwards.
    SpecialKey KeyDown -> gameState {player = movePlayer player ((-1) * verticalMovementStep)} -- Move the player downwards.
    Char 'r' -> initialState -- Reset the gamestate to the initalGamestate by pressing r.
    Char 's' -> let (updatedPlayer, newBulletList) = shoot player in gameState {player = updatedPlayer, bullets = bullets ++ newBulletList} -- Shoot when 's' is pressed, update the player cooldown and the current bullet list of the gamestate with the new bullet(s).
    Char 'p' -> gameState {phase =  Paused} -- Pause the game.
    _ -> gameState -- When another key is pressed, do nothing.
  -- When the game is paused, you can only reset the game, unpause or save the gamestate to JSON.
  Paused -> case pressedButton of
    Char 'r' -> initialState
    Char 'p' -> gameState {phase =  Playing} -- Unpause the game.
    _ -> gameState
    -- Char q -> -- Save gameState to JSON.
  -- When game over, you can only reset the game.
  GameOver -> case pressedButton of
    Char 'r' -> initialState
    _ -> gameState

-- Nothing happens if any other type of keyboard event is triggered.
keyboardInputHandler _ gameState = gameState


verticalMovementStep = 5.0 -- Amount of vertical movement of player based on input.