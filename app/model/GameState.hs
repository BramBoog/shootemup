module Model.GameState where

  import Model.Player
  import Model.Enemy
  import Model.Shooting (Bullet)
  import Model.General (Score)
  import Model.PowerUp

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
  }
