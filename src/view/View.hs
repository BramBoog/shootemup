{-# LANGUAGE NamedFieldPuns #-}
module View.View where

import Model.GameState
import Graphics.Gloss
import qualified Data.Map as Map
import Model.Player
import Model.Shooting
import Model.PowerUp
import Model.General
import Model.General

window :: Display
window = InWindow "Shoot 'Em Up" (800, 500) (0, 0)

-- Show a screen using Gloss with a player and powerup. Example, delete later.
examplePicture :: Picture
examplePicture = view GameState {isPaused = False, player = player1, enemies = ([], [], [], [], []), bullets = [], score = 1, powerUps = [powerUp1]}
    where 
        player1 = Player {playerPos = (-360, 100), speed = 5, weapon = Single, lives = 10}
        powerUp1 = BurstFire {burstFirePos = (-360, -200)}


-- Take an asset and use an - asset name to picture - mapping function to return a picture. All renderable objects are instances of the Show class.
render :: Show gameObject => Map.Map String Picture -> gameObject -> Picture
render assetNameToPicture gameObject = Map.findWithDefault defaultCircle (show gameObject) assetNameToPicture
-- If the key is not present in the dictionary, show a default circle.
    where 
        defaultCircle = circle defaultCircleSize
        defaultCircleSize = 30


enemySize = 50
playerSize = 50
powerupSize = 20
lineWidth = 6
bulletSize = 20

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
                                    ("Bullet", color azure (circleSolid bulletSize))
                                   ]


-- Given a picture and a gameObject, move the picture by the position of that gameObject.
translatePicture :: HasPosition gameObject => Picture -> gameObject -> Picture
translatePicture picture gameObject = translate x y picture
    where 
        (x, y) = pos gameObject


--Given a gameObject, return the right picture on the correct place.
givePicture :: (HasPosition gameObject, Show gameObject) => gameObject -> Picture
givePicture gameObject = translatePicture renderedGameObject gameObject
    where renderedGameObject = render asssetNameToPicture gameObject


-- Return all the pictures of the entire gamestate.
view :: GameState -> Picture
view GameState { player, enemies = (basicEnemyList, burstEnemyList, coneEnemyList, basicPlayerSeekingEnemyList,fastPlayerSeekingEnemyList), bullets, powerUps} = 
    Pictures (playerPicture : basicEnemyListPicture ++ burstEnemyListPicture ++ coneEnemyListPicture ++ basicPlayerSeekingEnemyListPicture ++ fastPlayerSeekingEnemyListPicture ++ bulletsPicture ++ powerUpsPicture)
        where 
            -- Lists of the pictures per game object type, translated to the right position
            playerPicture = givePicture player 
            basicEnemyListPicture = map givePicture burstEnemyList
            burstEnemyListPicture = map givePicture coneEnemyList
            coneEnemyListPicture = map givePicture basicPlayerSeekingEnemyList
            basicPlayerSeekingEnemyListPicture = map givePicture basicPlayerSeekingEnemyList
            fastPlayerSeekingEnemyListPicture = map givePicture fastPlayerSeekingEnemyList
            bulletsPicture = map givePicture bullets
            powerUpsPicture = map givePicture powerUps
