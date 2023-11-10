module View.Animations where

import Graphics.Gloss
import View.View
import Model.GameState
import Model.Movement

-- This module contains animations which play when certain events happen.

-- Data type which will be stored in a queue in gamestate for animation, the animations will be waiting to be played, each with a type and position.
data Animation = Animation {animationType :: String, animationPos :: Position}        

-- Take the animations from the queue and play the right type of animation at the right position.
handleAnimationQueue :: GameState -> IO GameState
handleAnimation Queue gs@GameState{animations@[]} = gs -- Do nothing is there are no animations to be played.
handleAnimationQueue gs@GameState{animations@(a1:as)} = 
do case animationType a1 of -- Otherwise, playe the next animation and the rest recursively.
        powerUpAnimation -> animate window black (animatePowerUp seconds pos) 
        bulletAnimation -> animate window black (animateBullet seconds pos)
        despawnAnimation -> animate window black (animateDespawn seconds pos)
    handleAnimationQueue gs@GameState{animations@(as)}
    return gs
    

animatePowerUp :: Float -> Position -> Picture
-- Given the animation type is powerup, this function takes the step size and starting position, and gives an updated picture each step. 
-- Animation: Four small yellow circles will move outwards from the starting position to four directions.
animatePowerUp seconds pos = Pictures $ map renderParticle (moveParticle ToTop seconds pos : moveParticle ToBottom seconds pos : moveParticle ToRight seconds pos : moveParticle ToLeft seconds pos) -- -- Function to create next frame of animation, seconds (Float) is the time since the program started, pos is the inital position.
    where
         -- This function akes a position and renders a particle there.
        renderParticle :: Position -> Picture 
        renderParticle pos@(x,y) = translate x y $ color yellow $ circleSolid particleSize

-- Data type that tells where animated object has to move to
data Direction = ToTop | ToBottom | ToLeft | ToRight

moveParticle :: Direction -> Float -> Position -> Position
-- This function updates the position of a particle, given a Direction that tells where the particle has to move to. d is the size of a step in this movement.
moveParticle direction seconds pos@(x, y) = let d = animationSpeed * seconds in case direction of 
    ToTop -> (x, y + d)
    ToBottom -> (x, y - d)
    ToRight -> (x + d, y)
    _ -> (x - d, y) -- ToLeft

-- Parameters
animationSpeed :: Float
animationSpeed = 5
particleSize :: Float
particleSize = 20
     
