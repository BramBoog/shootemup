{-# LANGUAGE NamedFieldPuns #-}
module View.View where

import Model.GameState
import Model.Parameters
import Model.Movement
import View.Window
import qualified Data.Map as Map
import Graphics.Gloss
import Model.Player
import View.Animations
import Data.List ((\\))

-- Take an asset and use an - asset name to picture - mapping function to return a picture. All renderable objects are instances of the Show class.
render :: Show gameObject => Map.Map String Picture -> gameObject -> Picture
render assetNameToPicture gameObject = Map.findWithDefault defaultCircle (show gameObject) assetNameToPicture
-- If the key is not present in the dictionary, show a default circle.
    where
        defaultCircle = circle defaultCircleSize
        defaultCircleSize = 30

-- Given a string refering to a data type, this function returns a corresponding picture for that data type.
asssetNameToPicture :: Map.Map String Picture
asssetNameToPicture = Map.fromList [("BasicEnemy", color red (circleSolid enemySize)),
                                    ("BurstEnemy", color chartreuse (circleSolid enemySize)),
                                    ("ConeEnemy", color yellow (circleSolid enemySize)),
                                    ("BasicPlayerSeekingEnemy", color rose (circleSolid enemySize)),
                                    ("FastPlayerSeekingEnemy", color violet (circleSolid enemySize)),
                                    ("Player", color blue (rectangleSolid playerSize playerSize)),
                                    ("BurstFire", color chartreuse (thickCircle powerupSize lineWidth)),
                                    ("ConeFire", color yellow (thickCircle powerupSize lineWidth)),
                                    ("SpeedBoost", color azure (thickCircle powerupSize lineWidth)),
                                    ("Bullet", color white (rectangleSolid bulletSizeX bulletSizeY))
                                   ]


-- Given a picture and a gameObject, move the picture by the position of that gameObject.
translatePicture :: HasPosition gameObject => Picture -> gameObject -> Picture
translatePicture picture gameObject = translate x y picture
    where
        (x, y) = pos gameObject


-- Given a gameObject, return the right picture on the correct place.
givePicture :: (HasPosition gameObject, Show gameObject) => gameObject -> Picture
givePicture gameObject = translatePicture renderedGameObject gameObject
    where renderedGameObject = render asssetNameToPicture gameObject


-- Given the gamestate, return a picture of the lives fo the player and their lives.
pictureLivesAndScore :: GameState -> Picture
pictureLivesAndScore gs@GameState{player = player, score = score} = Pictures [scorePicture, livesPicture]
    where
        scorePicture = scale textSize textSize $ translate (3.2 * screenMinX) (3 * screenSizeY) $ color white (Text ("Score:" ++ show score))-- Place the score in the left corner.
        livesPicture = scale textSize textSize $ translate (2.5 * screenMinX) (3 * screenSizeY) $ color white (Text ("Lives:" ++ show (lives player))) -- The lives are to the right of the score.
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

                
-- Remove an animation from the queue if its over.
removeAnimations :: GameStateTransform
removeAnimations gs@GameState{animations} = gs {animations = animations \\ removedAnimations}
    where
        removedAnimations = filter isAnimationOver animations
        currentTime = elapsedTime gs
        startingTime a = animationStart a
        isAnimationOver a = (currentTime - startingTime a) > animationLength


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
