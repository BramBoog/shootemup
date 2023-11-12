{-# LANGUAGE NamedFieldPuns #-}
module Model.GameState where
import Model.Movement (outOfBounds, Position, HasPosition (pos))
import Model.Player
import Model.Enemy
import Model.Shooting
import Model.PowerUp
import Model.Parameters
import Model.Randomness
import View.Window
import View.Animations
import Graphics.Gloss
import Data.List ((\\))
import GHC.Float (float2Double)
data GameState = GameState {
  phase :: GamePhase,
  player :: Player,
  enemies :: (
    [BasicEnemy],
    [BurstEnemy],
    [ConeEnemy],
    [BasicPlayerSeekingEnemy],
    [FastPlayerSeekingEnemy]
  ),
  bullets :: [Bullet],
  score :: Score,
  powerUps :: [PowerUp],
  animations :: [Animation],
  elapsedTime :: Float
}
data GamePhase = Playing | Paused | GameOver deriving (Eq, Show)
type Score = Int
type GameStateTransform   = GameState -> GameState
type GameStateTransformIO = GameState -> IO GameState
initialState :: GameState
initialState = GameState {
  phase = Playing,
  player = initialPlayer,
  enemies = initialEnemies,
  bullets = [],
  score = 0,
  powerUps = initialPowerUps,
  animations = [],
  elapsedTime = 0
}
  where 
    initialPlayer = Player {playerPos = (playerX, 0), speed = Normal, playerWeapon = Single, lives = 10, playerCooldown = 5}
    initialEnemies = ([], [BurstEnemy (300, 80) 1], [ConeEnemy (300, -20) 1], [], [FastPlayerSeekingEnemy (300, -80)])
    initialPowerUps = [BurstFire {burstFirePos = (playerX, -200)}]
-- Update the elapsedTime field of the GameState with the elapsed time given by the step function.
updateElapsedTime :: Float -> GameStateTransform
updateElapsedTime t gs = gs{elapsedTime = elapsedTime gs + t}
-- Move all bullets in the GameState.
moveBullets :: GameStateTransform
moveBullets gs = gs{bullets = map moveBullet (bullets gs)}
-- Move all enemies in the GameState.
enemiesMove :: GameStateTransform
enemiesMove gs@GameState{
  player,
  enemies = (basicEnemies, burstEnemies, coneEnemies, basicPlayerSeekingEnemies, fastPlayerSeekingEnemies)
} = gs{enemies = (
      map moveBasic basicEnemies,
      map moveBasic burstEnemies,
      map moveBasic coneEnemies,
      map (moveSeeking player) basicPlayerSeekingEnemies,
      map (moveSeeking player) fastPlayerSeekingEnemies
    )}
-- Despawn all enemies an bullets which have gone out of bounds.
despawnOutOfBounds :: GameStateTransform
despawnOutOfBounds gs@GameState{
  enemies = (basicEnemies, burstEnemies, coneEnemies, basicPlayerSeekingEnemies, fastPlayerSeekingEnemies),
  bullets
} = let basicOutOfBounds        = filter outOfBounds basicEnemies
        burstOutOfBounds        = filter outOfBounds burstEnemies
        coneOutOfBounds         = filter outOfBounds coneEnemies
        basicSeekingOutOfBounds = filter outOfBounds basicPlayerSeekingEnemies
        fastSeekingOutOfBounds  = filter outOfBounds fastPlayerSeekingEnemies
        bulletsOutOfBounds      = filter outOfBounds bullets
     in despawn basicOutOfBounds
      $ despawn burstOutOfBounds
      $ despawn coneOutOfBounds
      $ despawn basicSeekingOutOfBounds
      $ despawn fastSeekingOutOfBounds
      $ despawn bulletsOutOfBounds gs
-- Determines which enemies have been hit by bullets and despawns both them and the corresponding bullets.
killEnemies :: GameStateTransform
killEnemies gs@GameState{
  enemies = (basicEnemies, burstEnemies, coneEnemies, basicPlayerSeekingEnemies, fastPlayerSeekingEnemies),
  bullets
} = let (basicHit, bulletsHitBasic)               = hitByBulletsList basicEnemies bullets
        (burstHit, bulletsHitBurst)               = hitByBulletsList burstEnemies bullets
        (coneHit, bulletsHitCone)                 = hitByBulletsList coneEnemies bullets
        (basicSeekingHit, bulletsHitBasicSeeking) = hitByBulletsList basicPlayerSeekingEnemies bullets
        (fastSeekingHit, bulletsHitFastSeeking)   = hitByBulletsList fastPlayerSeekingEnemies bullets
        allBulletsHit                             = bulletsHitBasic
                                                 ++ bulletsHitBurst
                                                 ++ bulletsHitCone
                                                 ++ bulletsHitBasicSeeking
                                                 ++ bulletsHitFastSeeking
     in despawn basicHit
      $ despawn burstHit
      $ despawn coneHit
      $ despawn basicSeekingHit
      $ despawn fastSeekingHit
      $ despawn allBulletsHit gs
-- Lowers the shooting cooldowns by the elapsed time for the player an all enemies types that can shoot.
lowerCooldowns :: Float -> GameStateTransform
lowerCooldowns t gs@GameState{
  player,
  enemies = (basicEnemies, burstEnemies, coneEnemies, basicPlayerSeekingEnemies, fastPlayerSeekingEnemies)
} = let basicLowered   = map (lowerCooldown t) basicEnemies
        burstLowered   = map (lowerCooldown t) burstEnemies
        coneLowered    = map (lowerCooldown t) coneEnemies
        seekingLowered = map (lowerCooldown t) basicPlayerSeekingEnemies
     in gs{
       player = lowerCooldown t player,
       enemies = (basicLowered, burstLowered, coneLowered, seekingLowered, fastPlayerSeekingEnemies)
     }
-- Let all enemies shoot, if their cooldown allows. Add the new enemies (with reset cooldowns) and newly generated bullets to the new GameState
enemiesShoot :: GameStateTransform
enemiesShoot gs@GameState{
  enemies = (basicEnemies, burstEnemies, coneEnemies, basicPlayerSeekingEnemies, fastPlayerSeekingEnemies),
  bullets
} = let (newBasics, newBulletsBasic)     = allShoot basicEnemies
        (newBursts, newBulletsBurst)     = allShoot burstEnemies
        (newCones, newBulletsCone)       = allShoot coneEnemies
        (newSeekings, newBulletsSeeking) = allShoot basicPlayerSeekingEnemies
        allNewBullets                    = newBulletsBasic
                                        ++ newBulletsBurst
                                        ++ newBulletsCone
                                        ++ newBulletsSeeking
     in gs{
       enemies = (newBasics, newBursts, newCones, newSeekings, fastPlayerSeekingEnemies),
       bullets = bullets ++ allNewBullets
     }
-- Calculate all enemies and bullets which hit the player; despawn all of them and subtract a life from the player for each.
takeHitsPlayer :: GameStateTransform
takeHitsPlayer gs@GameState{
  player,
  enemies = (basicEnemies, burstEnemies, coneEnemies, basicPlayerSeekingEnemies, fastPlayerSeekingEnemies),
  bullets
} = let basicHit        = hitsOnPlayer player basicEnemies
        burstHit        = hitsOnPlayer player burstEnemies
        coneHit         = hitsOnPlayer player coneEnemies
        basicSeekingHit = hitsOnPlayer player basicPlayerSeekingEnemies
        fastSeekingHit  = hitsOnPlayer player fastPlayerSeekingEnemies
        bulletsHit      = hitsOnPlayer player bullets
        lostLives       = length basicHit
                        + length burstHit
                        + length coneHit
                        + length basicSeekingHit
                        + length fastSeekingHit
                        + length bulletsHit
     in despawn basicHit
      $ despawn burstHit
      $ despawn coneHit
      $ despawn basicSeekingHit
      $ despawn fastSeekingHit
      $ despawn bulletsHit
      $ subtractLives lostLives gs
     where
       subtractLives :: Int -> GameStateTransform
       subtractLives x gs@GameState{player} = gs{player = player {lives = lives player - x}}
gameOverIfNoLives :: GameStateTransform
gameOverIfNoLives gs@GameState{player} = if lives player <= 0 then gs{phase = GameOver}
                                         else gs
-- Randomly determine whether to spawn a new random enemy. If so, its type and starting y coordinate are randomly determined.
spawnNewEnemy :: GameStateTransformIO
spawnNewEnemy gs = do p <- generateProbability
                      if p <= spawnEnemyOnStepProbability then do r <- generateProbability
                                                                  spawnTransform <- chooseWithProb enemyProbDist
                                                                  spawnTransform gs
                      else return gs
  where
    -- Create a probability distribution of GameStateTranformIOs, each spawning a different enemy type
    enemyProbDist :: ProbDist GameStateTransformIO
    enemyProbDist = ProbDist (zip enemyProbs spawnEnemyTransforms)
    -- Makes each enemy type more likely to appear as time goes on, by increasing the probability modifier for each type, until it's capped at 1.
    -- This means at some point "easier" enemy types stop becoming more common, while "harder" enemy types are still increasing in frequency.
    enemyProbs = map modifiyProbByElapsedTimeAndCap enemyProbModifiers
    modifiyProbByElapsedTimeAndCap p = (float2Double (elapsedTime gs) / timeToHardestGameState * p) `min` 1
    enemyProbModifiers = [basicEnemyModifier, burstEnemyModifier, coneEnemyModifier, basicSeekingEnemyModifier, fastSeekingEnemyModifier]
    spawnEnemyTransforms = [
        spawnInGameState (spawn :: IO BasicEnemy),
        spawnInGameState (spawn :: IO BurstEnemy),
        spawnInGameState (spawn :: IO ConeEnemy),
        spawnInGameState (spawn :: IO BasicPlayerSeekingEnemy),
        spawnInGameState (spawn :: IO FastPlayerSeekingEnemy)
      ]
-- Randomly determine whether to spawn a new random PowerUp.
spawnNewPowerUp :: GameStateTransformIO
spawnNewPowerUp gs = do p <- generateProbability
                        if p <= spawnPowerUpOnStepProbability then spawnInGameState spawnPowerUp gs
                        else return gs


-- Combine all GameStateTransforms that occur on a time step into one, if the game is in Playing phase.
updateOnStep :: Float -> GameStateTransformIO
updateOnStep t gs@GameState{phase} = case phase of
                                       Playing -> update
                                       _ -> return gs
  where update = let updatedTimesGS = updateTimes gs              -- first update the times in the gamestate
                  in do spawnedNewGS <- spawnedNew updatedTimesGS -- then optionally spawn a new enemy and powerup
                                                                  -- read these in reverse, since that's the order they're applied
                        return $ gameOverIfNoLives                -- finally, transition to GameOver phase if player has lost all lives
                               $ despawnOutOfBounds               -- despawn all bullets and enemies which have gone out of bounds
                               $ takeHitsPlayer                   -- and check again if the player has been hit
                               $ enemiesMove                      -- then move the enemies
                               $ takeHitsPlayer                   -- having moved the bullets, check if the player has been hit
                               $ killEnemies                      -- after moving bullets, check if that killed any enemies
                               $ moveBullets
                               $ enemiesShoot spawnedNewGS
                                        
        updateTimes = lowerCooldowns t
                    . updateElapsedTime t
        spawnedNew gs' = do spawnedPowerUpGS <- spawnNewPowerUp gs'
                            spawnNewEnemy spawnedPowerUpGS
-- For all objects which can be despawned from the GameState.
class Despawnable a where
  despawn :: [a] -> GameStateTransform
-- Use list difference operator \\ to remove elements to despawn from each list
instance Despawnable BasicEnemy where
  despawn es gs = let (basics, bursts, cones, basicseekings, fastseekings) = enemies gs
                  in gs {enemies = (basics \\ es, bursts, cones, basicseekings, fastseekings), animations = despawnAnimations gs es ++ animations gs}
instance Despawnable BurstEnemy where
  despawn es gs = let (basics, bursts, cones, basicseekings, fastseekings) = enemies gs
                  in gs {enemies = (basics, bursts \\ es, cones, basicseekings, fastseekings), animations = despawnAnimations gs es ++ animations gs}
instance Despawnable ConeEnemy where
  despawn es gs = let (basics, bursts, cones, basicseekings, fastseekings) = enemies gs
                  in gs {enemies = (basics, bursts, cones \\ es, basicseekings, fastseekings), animations = despawnAnimations gs es ++ animations gs}
instance Despawnable BasicPlayerSeekingEnemy where
  despawn es gs = let (basics, bursts, cones, basicseekings, fastseekings) = enemies gs
                  in gs {enemies = (basics, bursts, cones, basicseekings \\ es, fastseekings), animations = despawnAnimations gs es ++ animations gs}
instance Despawnable FastPlayerSeekingEnemy where
  despawn es gs = let (basics, bursts, cones, basicseekings, fastseekings) = enemies gs
                  in gs {enemies = (basics, bursts, cones, basicseekings, fastseekings \\ es), animations = despawnAnimations gs es ++ animations gs}
instance Despawnable Bullet where
  despawn bs gs = gs {bullets = bullets gs \\ bs}
--Given a list of enemies, return a list of despawnAnimations, each with a certain position corresponding to that of each enemy.
despawnAnimations :: (Despawnable a, HasPosition a) => GameState -> [a] -> [Animation]
despawnAnimations gs es = map (\ e -> Animation {animationType = DespawnAnimation, animationPos = pos e, animationStart = elapsedTime gs}) es
-- For all objects which can be spawned into the GameState with a random component.
class Spawnable a where
  spawnInGameState :: IO a -> GameStateTransformIO
instance Spawnable BasicEnemy where
  spawnInGameState s gs@GameState{enemies = (basics, bursts, cones, basicseekings, fastseekings)} =
    do e <- s
       return gs{enemies = (e : basics, bursts, cones, basicseekings, fastseekings)}
instance Spawnable BurstEnemy where
  spawnInGameState s gs@GameState{enemies = (basics, bursts, cones, basicseekings, fastseekings)} =
    do e <- s
       return gs{enemies = (basics, e : bursts, cones, basicseekings, fastseekings)}
instance Spawnable ConeEnemy where
  spawnInGameState s gs@GameState{enemies = (basics, bursts, cones, basicseekings, fastseekings)} =
    do e <- s
       return gs{enemies = (basics, bursts, e : cones, basicseekings, fastseekings)}
instance Spawnable BasicPlayerSeekingEnemy where
  spawnInGameState s gs@GameState{enemies = (basics, bursts, cones, basicseekings, fastseekings)} =
    do e <- s
       return gs{enemies = (basics, bursts, cones, e : basicseekings, fastseekings)}
instance Spawnable FastPlayerSeekingEnemy where
  spawnInGameState s gs@GameState{enemies = (basics, bursts, cones, basicseekings, fastseekings)} =
    do e <- s
       return gs{enemies = (basics, bursts, cones, basicseekings, e : fastseekings)}
instance Spawnable PowerUp where
  spawnInGameState s gs@GameState{powerUps} =
    do p <- s
       return gs{powerUps = p : powerUps}