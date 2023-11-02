module Model.GameState where

import Model.Player
import Model.Enemy
import Model.Shooting
import Model.PowerUp
import Data.List
import Data.Maybe

data GameState = GameState {
  isPaused :: Bool,
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
  powerUps :: [PowerUp]
} deriving Show

type Score = Int
type GameStateTransform = GameState -> GameState

despawnBullets :: [Bullet] -> GameStateTransform

despawnEnemies :: (Enemy a, Despawnable a) => [a] -> GameStateTransform
despawnEnemies gs = let (basics, bursts, cones, basicseekings, fastseekings) = enemies gs
                        bs = bullets gs
                     in undefined
  where
    hitEnemiesOfType :: (Enemy a, Despawnable a) => [a] -> [Bullet] -> ([a], [Bullet])
    hitEnemiesOfType enemies bs = let hits = catMaybes [hit e b | e <- enemies, b <- bs]
                                   in (map fst hits, map snd hits)

takeHitsPlayer :: GameStateTransform

updateOnStep :: GameStateTransform

class Despawnable a where
  despawn :: GameState -> [a] -> GameState

-- Use list difference operator \\ to remove enemies to despawn from each enemy list
instance Despawnable BasicEnemy where
  despawn gs es = let (basics, bursts, cones, basicseekings, fastseekings) = enemies gs
                   in gs {enemies = (basics \\ es, bursts, cones, basicseekings, fastseekings)}

instance Despawnable BurstEnemy where
  despawn gs es = let (basics, bursts, cones, basicseekings, fastseekings) = enemies gs
                   in gs {enemies = (basics, bursts \\ es, cones, basicseekings, fastseekings)}

instance Despawnable ConeEnemy where
  despawn gs es = let (basics, bursts, cones, basicseekings, fastseekings) = enemies gs
                   in gs {enemies = (basics, bursts, cones \\ es, basicseekings, fastseekings)}

instance Despawnable BasicPlayerSeekingEnemy where
  despawn gs es = let (basics, bursts, cones, basicseekings, fastseekings) = enemies gs
                   in gs {enemies = (basics, bursts, cones, basicseekings \\ es, fastseekings)}

instance Despawnable FastPlayerSeekingEnemy where
  despawn gs es = let (basics, bursts, cones, basicseekings, fastseekings) = enemies gs
                   in gs {enemies = (basics, bursts, cones, basicseekings, fastseekings \\ es)}

instance Despawnable Bullet where
  despawn gs bs = gs {bullets = bullets gs \\ bs}
