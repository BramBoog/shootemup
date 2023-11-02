{-# LANGUAGE NamedFieldPuns #-}
module View.View where

import Model.GameState
import Graphics.Gloss
import qualified Data.Map as Map
import Model.Player
import Model.Shooting
import Model.PowerUp

-- Show a screen using Gloss with a player and powerup. Example, delete later.
window :: Display
window = InWindow "Shoot 'Em Up" (500, 500) (10, 10)

picture :: Picture
picture = view asssetNameToPicture GameState {player = player1, powerUps = [powerUp1]}
    where 
        player1 = Player {playerPos = (20, 20), speed = 5, weapon = Single, lives = 10}
        powerUp1 = BurstFire {burstFirePos = (30, 30)}


-- Take an asset and use an - asset name to picture - mapping function to return a picture. All renderable objects are instances of the Show class.
render :: Show a => Map.Map String Picture -> a -> Picture
render assetNameToPicture a = Map.findWithDefault defaultCircle (show a) assetNameToPicture
-- If the key is not present in the dictionary, show a default circle.
    where 
        defaultCircle = circle defaultCircleSize
        defaultCircleSize = 0.5


enemySize = 0.5
playerSize = 0.5
powerupSize = 0.3
lineWidth = 0.2
bulletSize = 0.1

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


-- Return all the pictures of the entire gamestate.
view :: Map.Map String Picture -> GameState -> Picture
view assetNameToPicture GameState { player, enemies = (basicEnemyList, burstEnemyList, coneEnemyList, basicPlayerSeekingEnemyList,fastPlayerSeekingEnemyList), bullets, powerUps} = 
    Pictures $ map (translate xPos yPos) $ map (render assetNameToPicture) $ player : basicEnemyList ++ burstEnemyList ++ coneEnemyList ++ basicPlayerSeekingEnemyList ++ fastPlayerSeekingEnemyList ++ bullets ++ powerUps
        where 
            xPos = fst pos
            yPos = snd pos
