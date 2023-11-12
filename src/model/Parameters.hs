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
basicEnemyHorizontalSpeed = 8
basicEnemyVerticalSpeed = 5
fastEnemyHorizontalSpeed = 13
fastEnemyVerticalSpeed = 8

playerNormalVerticalSpeed, playerBoostedVerticalSpeed :: Float
playerNormalVerticalSpeed = 10
playerBoostedVerticalSpeed = 20

bulletHorizontalSpeed, bulletVerticalSpeed :: Float
bulletHorizontalSpeed = 15
bulletVerticalSpeed = 7.5

-- The space between the shooting object and the bullet, and the space between each bullet in a burst fire.
standardBulletDisplacement, burstBulletDisplacement :: Float
standardBulletDisplacement = 25
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
enemyShootingCooldown = 5
playerShootingCooldown = 2

powerUpDuration :: Float
powerUpDuration = 10

-- Spawning probabilities

timeToHardestGameState :: Double
timeToHardestGameState = 600 -- 10 minutes

spawnPowerUpOnStepProbability :: Double
spawnPowerUpOnStepProbability = 0.007

spawnEnemyOnStepProbability :: Double
spawnEnemyOnStepProbability = 0.02

basicEnemyModifier, burstEnemyModifier, coneEnemyModifier, basicSeekingEnemyModifier, fastSeekingEnemyModifier :: Double
basicEnemyModifier = 1.8
burstEnemyModifier = 1.6
coneEnemyModifier = 1.4
basicSeekingEnemyModifier = 1.2
fastSeekingEnemyModifier = 1


-- Animation

animationLength :: Float
animationLength = 3
particleSize :: Float
particleSize = 10
animationSize :: Float
animationSize = 30
