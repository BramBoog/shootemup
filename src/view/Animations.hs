{-# LANGUAGE NamedFieldPuns #-}
module View.Animations where

import Graphics.Gloss
import Model.Movement (Position, Direction (ToTop, ToBottom, ToLeft, ToRight))

-- This module contains animations which play when certain events happen.

-- Data type which will be stored in a queue in gamestate for animation, the animations will be waiting to be played, each with a type and position. The animationStart is the time when the animation has to be start playing.
data Animation = Animation {animationType :: AnimationType, animationPos :: Position, animationStart :: Float} deriving Eq
-- Depending on the animationType, different particles will be used for the animation.    
data AnimationType = PowerUpAnimation | BulletAnimation | DespawnAnimation deriving Eq

--moveParticle :: Direction -> Float -> Position -> Position
-- This function updates the position of a particle, given a Direction that tells where the particle has to move to. d is the size of a step in this movement.
--moveParticle direction seconds pos@(x, y) = let d = animationSpeed * seconds in case direction of
--    ToTop -> (x, y + d)
--    ToBottom -> (x, y - d)
--    ToRight -> (x + d, y)
--    _ -> (x - d, y) -- ToLeft

--playAnimation :: AnimationType -> Position -> Float -> Picture
-- Given the animation type, this function takes the step size and starting position, and gives an updated picture each step. 
-- Animation: Four small shapes will move outwards from the starting position to four directions.
--playAnimation animationType pos seconds = Pictures $ map (renderParticle animationType) [moveParticle ToTop seconds pos, moveParticle ToBottom seconds pos, moveParticle ToRight seconds pos, moveParticle ToLeft seconds pos] -- Function to create next frame of animation, seconds (Float) is the time since the program started, pos is the inital position.



