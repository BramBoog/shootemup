module Model.Parameters where

-- movement and position

screenSizeX, screenSizeY, screenMinX, screenMaxX, screenMinY, screenMaxY :: Float
screenSizeX = 800
screenMinX = -screenSizeX
screenMaxX = screenSizeX
screenSizeY = 450
screenMinY = -screenSizeY
screenMaxY = screenSizeY

playerX :: Float
playerX = screenMinX + 50

basicEnemyHorizontalSpeed, basicEnemyVerticalSpeed, fastEnemyHorizontalSpeed, fastEnemyVerticalSpeed :: Float
basicEnemyHorizontalSpeed = 2
basicEnemyVerticalSpeed = 1
fastEnemyHorizontalSpeed = 3
fastEnemyVerticalSpeed = 1.5

playerNormalVerticalSpeed, playerBoostedVerticalSpeed :: Float
playerNormalVerticalSpeed = 2
playerBoostedVerticalSpeed = 3

bulletHorizontalSpeed, bulletVerticalSpeed :: Float
bulletHorizontalSpeed = 4
bulletVerticalSpeed = 2

-- The space between the shooting object and the bullet, and the space between each bullet in a burst fire.
standardBulletDisplacement, burstBulletDisplacement :: Float
standardBulletDisplacement = 0.02
burstBulletDisplacement = 20

-- Size of rendered objects
enemySize, playerSize, powerupSize, lineWidth, bulletSize :: Float
enemySize = 20
playerSize = 50
powerupSize = 20
lineWidth = 6
bulletSize = 7

-- time

enemyShootingCooldown, playerShootingCooldown :: Float
enemyShootingCooldown = 4
playerShootingCooldown = 3

powerUpDuration :: Float
powerUpDuration = 2

-- Spawning probabilities

timeToHardestGameState :: Double
timeToHardestGameState = 600 -- 10 minutes

spawnPowerUpOnStepProbability :: Double
spawnPowerUpOnStepProbability = 0.01

spawnEnemyOnStepProbability :: Double
spawnEnemyOnStepProbability = 0.05

basicEnemyModifier, burstEnemyModifier, coneEnemyModifier, basicSeekingEnemyModifier, fastSeekingEnemyModifier :: Double
basicEnemyModifier = 1.8
burstEnemyModifier = 1.6
coneEnemyModifier = 1.4
basicSeekingEnemyModifier = 1.2
fastSeekingEnemyModifier = 1


-- Animation

animationLength :: Float
animationLength = 4
particleSize :: Float
particleSize = 20
animationSize :: Float
animationSize = 50
