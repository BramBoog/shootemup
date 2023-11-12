{-# LANGUAGE NamedFieldPuns #-}
module View.Animations where

import Graphics.Gloss
import Model.Movement (Position, Direction (ToTop, ToBottom, ToLeft, ToRight))
import Model.Parameters

-- This module contains animations which play when certain events happen.

-- Data type which will be stored in a queue in gamestate for animation, the animations will be waiting to be played, each with a type and position. The animationStart is the time when the animation has to be start playing.
data Animation = Animation {animationType :: AnimationType, animationPos :: Position, animationStart :: Float} deriving Eq
-- Depending on the animationType, different particles will be used for the animation.    
data AnimationType = PowerUpAnimation | BulletAnimation | DespawnAnimation deriving Eq

animationsOver :: Float -> [Animation] -> [Animation]
animationsOver t = filter isAnimationOver
  where
    startingTime a = animationStart a
    isAnimationOver a = (t - startingTime a) > animationLength
