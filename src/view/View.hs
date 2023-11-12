{-# LANGUAGE NamedFieldPuns #-}
module View.View where

import Model.GameState
import Model.Parameters
import Model.Movement
import View.Window
import qualified Data.Map as Map
import Graphics.Gloss
import Model.Player


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

-- Return all the pictures of the entire gamestate.
viewPure :: GameState -> Picture
viewPure gs@GameState{ player, enemies = (basicEnemyList, burstEnemyList, coneEnemyList, basicPlayerSeekingEnemyList,fastPlayerSeekingEnemyList), bullets, powerUps} =
    Pictures (pictureLivesAndScore gs : playerPicture : basicEnemyListPicture ++ burstEnemyListPicture ++ coneEnemyListPicture ++ basicPlayerSeekingEnemyListPicture ++ fastPlayerSeekingEnemyListPicture ++ bulletsPicture ++ powerUpsPicture)
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
