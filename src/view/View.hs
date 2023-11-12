{-# LANGUAGE NamedFieldPuns #-}
module View.View where

import Model.GameState (GameState (GameState, player, enemies, bullets, powerUps))
import Model.Parameters
import Model.Player
import Model.PowerUp
import Model.Movement
import Model.Shooting (Bullet)
import Model.Enemy

import GHC.Float (float2Int)
import qualified Data.Map as Map
import Graphics.Gloss

window :: Display
window = InWindow "Shoot 'Em Up" (2 * float2Int screenSizeX, 2 * float2Int screenSizeY) (0, 0)


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
    toPicture _ = color white (rectangleSolid bulletSizeX bulletSizeY)

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


-- Return all the pictures of the entire gamestate.
viewPure :: GameState -> Picture
viewPure GameState { player, enemies = (basicEnemyList, burstEnemyList, coneEnemyList, basicPlayerSeekingEnemyList,fastPlayerSeekingEnemyList), bullets, powerUps} = 
    Pictures (playerPicture : basicEnemyListPicture ++ burstEnemyListPicture ++ coneEnemyListPicture ++ basicPlayerSeekingEnemyListPicture ++ fastPlayerSeekingEnemyListPicture ++ bulletsPicture ++ powerUpsPicture)
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
