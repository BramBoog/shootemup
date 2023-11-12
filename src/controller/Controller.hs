{-# LANGUAGE NamedFieldPuns #-}
module Controller.Controller where

import Model.GameState
import Model.Player (movePlayer)
import Model.Shooting
import Model.Movement (HasPosition (pos), Direction (ToTop, ToBottom))
import View.Animations

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

step :: Float -> GameStateTransformIO
step = updateOnStep

input :: Event -> GameStateTransformIO
input event gamestate = return (keyboardInputHandler event gamestate)

-- This function takes an event, a certain keyboard input, and a current gamestate, and returns the new gamestate.
keyboardInputHandler :: Event -> GameState -> GameState
keyboardInputHandler (EventKey pressedButton _ _ _) gameState@GameState{player, bullets, animations}  = case phase gameState of
  -- If the gamephase is 'playing', the following input is considered:
  Playing -> case pressedButton of
    SpecialKey KeyUp -> gameState {player = movePlayer ToTop player} -- Move the player upwards.
    SpecialKey KeyDown -> gameState {player = movePlayer ToBottom player} -- Move the player downwards.
    Char 'r' -> initialState -- Reset the gamestate to the initalGamestate by pressing r.
    Char 's' -> let (updatedPlayer, newBulletList) = shoot player
                    newAnimation = Animation {animationType = BulletAnimation, animationPos = pos player} -- Add the bullet animation to the animationQueue.
                in gameState {player = updatedPlayer, bullets = bullets ++ newBulletList, animations = newAnimation : animations} -- Shoot when 's' is pressed, update the player cooldown and the current bullet list of the gamestate with the new bullet(s).
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
