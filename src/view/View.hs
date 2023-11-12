{-# LANGUAGE NamedFieldPuns #-}
module View.View where

import Model.GameState (GameState (GameState, score, player, enemies, bullets, powerUps, elapsedTime, animations))
import Model.Parameters
import Model.Player
import Model.PowerUp
import Model.Movement
import Model.Shooting (Bullet)
import Model.Enemy
import View.Window
import View.Animations

import GHC.Float (float2Int)
import qualified Data.Map as Map
import Graphics.Gloss

-- Defines a Picture for each object
class Renderable a where
    toPicture :: a -> Picture

instance Renderable Player where
    toPicture _ = color blue (rectangleSolid playerSize playerSize)

instance Renderable PowerUp where
    toPicture (BurstFire _) = color chartreuse (thickCircle powerupSize lineWidth)
    toPicture (ConeFire _) = color yellow (thickCircle powerupSize lineWidth)
    toPicture (SpeedBoost _) = color azure (thickCircle powerupSize lineWidth)

instance Renderable Bullet where
    toPicture _ = color white (rectangleSolid bulletSize bulletSize)

instance Renderable BasicEnemy where
    toPicture _ = color red (circleSolid enemySize)

instance Renderable BurstEnemy where
    toPicture _ = color chartreuse (circleSolid enemySize)

instance Renderable ConeEnemy where
    toPicture _ = color yellow (circleSolid enemySize)

instance Renderable BasicPlayerSeekingEnemy where
    toPicture _ = color rose (circleSolid enemySize)

instance Renderable FastPlayerSeekingEnemy where
    toPicture _ = color violet (circleSolid enemySize)

-- Given a picture and a gameObject, move the picture by the position of that gameObject.
translatePicture :: HasPosition gameObject => Picture -> gameObject -> Picture
translatePicture picture gameObject = translate x y picture
    where
        (x, y) = pos gameObject


--Given a gameObject, return the right picture on the correct place.
givePicture :: (HasPosition gameObject, Renderable gameObject) => gameObject -> Picture
givePicture gameObject = translatePicture renderedGameObject gameObject
    where renderedGameObject = toPicture gameObject


-- Given the gamestate, return a picture of the lives fo the player and their lives.
pictureLivesAndScore :: GameState -> Picture
pictureLivesAndScore gs@GameState{player = player, score = score} = Pictures [scorePicture, livesPicture]
    where
        scorePicture = scale textSize textSize $ translate (3.2 * screenMinX) (3 * screenMaxY) $ color white (Text ("Score:" ++ show score))-- Place the score in the left corner.
        livesPicture = scale textSize textSize $ translate (2.5 * screenMinX) (3 * screenMaxY) $ color white (Text ("Lives:" ++ show (lives player))) -- The lives are to the right of the score.
        textSize = 0.3


-- Given the gamestate with the animationqueue, return the current animation frames based on the difference between AnimationStart and elapsedTime.
renderAnimations :: GameState -> Picture
renderAnimations gs@GameState{animations} = Pictures $ map renderOneAnimation animations
    where
        renderOneAnimation :: Animation -> Picture
        renderOneAnimation animation = renderParticles animation relativeFactor difference -- During the animation, show the particles at the right place.
                                     
            where
                startingTime = animationStart animation
                currentTime = elapsedTime gs
                difference = currentTime - startingTime
                -- This factor shows how far in the animation we are.
                relativeFactor = animationSize * currentTime / animationLength


-- This function takes an animation and renders a particle there, where the position is based on the difference between elaspedTime and animationStart.
renderParticles :: Animation -> Float -> Float -> Picture
renderParticles animation relativeFactor difference = Pictures [topParticle, bottomParticle, leftParticle, rightParticle]
    where
        -- Based on the type of animation, give the corresponding shape and colour of the particles.
        (aType, aPos@(x,y))  = (animationType animation, animationPos animation)
        topParticle = uncurry translate (relativePos aPos relativeFactor ToTop) shapeAndColourParticles
        bottomParticle = uncurry translate (relativePos aPos relativeFactor ToBottom) shapeAndColourParticles
        leftParticle = uncurry translate (relativePos aPos relativeFactor ToLeft) shapeAndColourParticles
        rightParticle = uncurry translate (relativePos aPos relativeFactor ToRight) shapeAndColourParticles
        shapeAndColourParticles = case aType of
            PowerUpAnimation -> color yellow $ circleSolid particleSize
            BulletAnimation -> color blue $ circleSolid (particleSize/2)
            DespawnAnimation -> color red $ rectangleSolid particleSize particleSize

-- Based on the direction and time since the start of the animation, return a relative position of a particle.
relativePos :: Position -> Float -> Direction -> Position
relativePos aPos@(x, y) relativeFactor direction = case direction of
    ToTop -> (x, y + relativeFactor)
    ToBottom -> (x, y - relativeFactor)
    ToRight -> (x + relativeFactor, y)
    ToLeft -> (x - relativeFactor, y)


-- Return all the pictures of the entire gamestate.
viewPure :: GameState -> Picture
viewPure gs@GameState{ player, enemies = (basicEnemyList, burstEnemyList, coneEnemyList, basicPlayerSeekingEnemyList,fastPlayerSeekingEnemyList), bullets, powerUps} =
    Pictures (renderAnimations gs : pictureLivesAndScore gs : playerPicture : basicEnemyListPicture ++ burstEnemyListPicture ++ coneEnemyListPicture ++ basicPlayerSeekingEnemyListPicture ++ fastPlayerSeekingEnemyListPicture ++ bulletsPicture ++ powerUpsPicture)
        where
            -- Lists of the pictures per game object type, translated to the right position
            playerPicture = givePicture player
            basicEnemyListPicture = map givePicture basicEnemyList
            burstEnemyListPicture = map givePicture burstEnemyList
            coneEnemyListPicture = map givePicture coneEnemyList
            basicPlayerSeekingEnemyListPicture = map givePicture basicPlayerSeekingEnemyList
            fastPlayerSeekingEnemyListPicture = map givePicture fastPlayerSeekingEnemyList
            bulletsPicture = map givePicture bullets
            powerUpsPicture = map givePicture powerUps

view :: GameState -> IO Picture
view = return . viewPure
