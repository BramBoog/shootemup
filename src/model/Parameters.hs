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

-- change later to generate randomly
randomY :: Float
randomY = 0.5

-- time

enemyShootingCooldown, playerShootingCooldown :: Float
enemyShootingCooldown = 4
playerShootingCooldown = 3
