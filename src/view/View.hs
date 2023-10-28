{-# LANGUAGE NamedFieldPuns #-}
module View.View where

import Model.GameState
import Graphics.Gloss
import qualified Data.Map as Map

-- Show a screen using Gloss with a circle. Example, delete later.
window :: Display
window = InWindow "Text" (500, 500) (10, 10)

picture :: Picture
picture = circle 100

assets = Map.fromList

-- Class of all renderable objects
class Renderable a where
    -- Take an asset and use an - asset name to picture - mapping function to return a picture.
    render :: Map.Map String Picture -> a -> Picture
    render assetNameToPicture = assetNameToPicture (show a)


enemySize = 0.5
playerSize = 0.5
powerupSize = 0.3
lineWidth = 0.2
bulletSize = 0.1

-- Given a string refering to a data type, this function returns a corresponding picture for that data type.
asssetNameToPicture :: Map.Map String Picture
asssetNameToPicture = fromlist [("BasicEnemy", color red circleSolid enemySize),
                                ("BurstEnemy", color green circleSolid enemySize)
                                ("ConeEnemy", color yellow circleSolid enemySize)
                                ("BasicPlayerSeekingEnemy", color pink circleSolid enemySize)
                                ("FastPlayerSeekingEnemy", color purple circleSolid enemySize)
                                ("Player", color blue rectangleSolid playerSize playerSize)  
                                ("BurstFire", color green thickCircle powerupSize lineWidth)
                                ("ConeFire", color yellow thickCircle powerupSize lineWidth)
                                ("SpeedBoost", color azure thickCircle powerupSize lineWidth)
                                ("Bullet", color azure circleSolid bulletSize)
                               ]


-- Return all the pictures of the entire gamestate.
view :: Map.Map String Picture -> GameState -> Picture
view assetNameToPicture GameState { player, enemies = (BasicEnemy, BurstEnemy, ConeEnemy, BasicPlayerSeekingEnemy, FastPlayerSeekingEnemy), bullets, powerups} = 
    Pictures $ map (render assetNameToPicture) $ player : BasicEnemy ++ BurstEnemy ++ ConeEnemy ++ BasicPlayerSeekingEnemy ++ FastPlayerSeekingEnemy ++ bullets ++ powerups