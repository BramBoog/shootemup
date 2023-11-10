module Model.Parameters where

-- movement and position

screenMinX, screenMaxX, screenMinY, screenMaxY :: Float
screenMinX = -400
screenMaxX = 400
screenMinY = -250
screenMaxY = 250

basicEnemyHorizontalSpeed, basicEnemyVerticalSpeed, fastEnemyHorizontalSpeed, fastEnemyVerticalSpeed :: Float
basicEnemyHorizontalSpeed = 2
basicEnemyVerticalSpeed = 1
fastEnemyHorizontalSpeed = 3
fastEnemyVerticalSpeed = 1.5

playerNormalVerticalSpeed, playerBoostedVerticalSpeed :: Float
playerNormalVerticalSpeed = 1
playerBoostedVerticalSpeed = 1.5

bulletHorizontalSpeed, bulletVerticalSpeed :: Float
bulletHorizontalSpeed = 4
bulletVerticalSpeed = 2

-- The space between the shooting object and the bullet, and the space between each bullet in a burst fire.
standardBulletDisplacement, burstBulletDisplacement :: Float
standardBulletDisplacement = 0.02
burstBulletDisplacement = 20


-- time
enemyShootingCooldown, playerShootingCooldown :: Float
enemyShootingCooldown = 4
playerShootingCooldown = 3


-- Spawning probabilities

timeToHardestGameState :: Float
timeToHardestGameState = 600 -- 10 minutes

spawnEnemyOnStepProbabilityModifier :: Float
spawnEnemyOnStepProbabilityModifier = 0.3

burstEnemyThreshold, coneEnemyThreshold, basicSeekingEnemyThreshold, fastSeekingEnemyThreshold :: Float
burstEnemyThreshold = 0.1
coneEnemyThreshold = 0.2
basicSeekingEnemyThreshold = 0.3
fastSeekingEnemyThreshold = 0.4
