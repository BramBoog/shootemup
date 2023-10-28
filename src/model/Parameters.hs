module Model.Parameters where

-- movement and position

screenMin :: Float
screenMin = -1
screenMax :: Float
screenMax = 1

basicEnemyHorizontalSpeed :: Float
basicEnemyHorizontalSpeed = 0.05
basicEnemyVerticalSpeed :: Float
basicEnemyVerticalSpeed = 0.03
fastEnemyHorizontalSpeed :: Float
fastEnemyHorizontalSpeed = 0.1
fastEnemyVerticalSpeed :: Float
fastEnemyVerticalSpeed = 0.06

playerNormalVerticalSpeed :: Float
playerNormalVerticalSpeed = 0.04
playerBoostedVerticalSpeed :: Float
playerBoostedVerticalSpeed = 0.8

bulletHorizontalSpeed :: Float
bulletHorizontalSpeed = 0.1
bulletVerticalSpeed :: Float
bulletVerticalSpeed = 0.06

-- change later to generate randomly
randomY :: Float
randomY = 0.5

-- time

enemyShootingCooldown :: Float
enemyShootingCooldown = 4
playerShootingCooldown :: Float
playerShootingCooldown = 3
