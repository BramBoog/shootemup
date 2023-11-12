{-# LANGUAGE NamedFieldPuns #-}
module Controller.Controller where

import Controller.FileSystem
import Model.GameState
import Model.Player (movePlayer)
import Model.Shooting
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

step :: Float -> GameStateTransformIO
step = updateOnStep

-- This function takes an event, a certain keyboard input, and a current gamestate, and returns the new gamestate.
keyboardInputHandler :: Event -> GameStateTransformIO
keyboardInputHandler (EventKey pressedButton keyState _ _) gameState@GameState{phase, player, bullets} = case phase of
  -- If the gamephase is 'playing', the following input is considered:
  Playing -> case (pressedButton, keyState) of
    (SpecialKey KeyUp, _) -> return gameState {player =  movePlayer player verticalMovementStep} -- Move the player upwards.
    (SpecialKey KeyDown, _) -> return gameState {player = movePlayer player ((-1) * verticalMovementStep)} -- Move the player downwards.
    (Char 'r', Down) -> return initialState -- Reset the gamestate to the initalGamestate by pressing r.
    (Char 's', _) -> let (updatedPlayer, newBulletList) = shoot player in return gameState {player = updatedPlayer, bullets = bullets ++ newBulletList} -- Shoot when 's' is pressed, update the player cooldown and the current bullet list of the gamestate with the new bullet(s).
    (Char 'p', Down) -> return gameState {phase =  Paused} -- Pause the game.
    _ -> return gameState -- When another key is pressed, do nothing.
  -- When the game is paused, you can only reset the game, unpause or save the gamestate to JSON.
  Paused -> case (pressedButton, keyState) of
    (Char 'r', Down) -> return initialState
    (Char 'p', Down) -> return gameState {phase =  Playing} -- Unpause the game.
    (Char 'q', Down) -> do saveGameState gameState -- Save gameState to JSON.
                           return gameState
    _ -> return gameState
  -- When game over, you can only reset the game.
  GameOver -> case pressedButton of
    Char 'r' -> return initialState
    _ -> return gameState

-- Nothing happens if any other type of keyboard event is triggered.
keyboardInputHandler _ gameState = return gameState


verticalMovementStep = 5.0 -- Amount of vertical movement of player based on input.