module Model.Movement (
  Position,
  Vector,
  move,
  HasPosition (pos, hitboxSize, hit, outOfBounds),
  Direction (ToTop, ToBottom, ToRight, ToLeft)
) where

import Model.Parameters (screenMinX, screenMaxX, screenMinY, screenMaxY, enemySize, playerSize, powerupSize, bulletSize)
import qualified Data.Map as Map


type Position = (Float, Float)
type Vector   = (Float, Float) -- x and y component
data Square = Square {bottomLeft :: Position, width :: Float} -- A square is defined by the position of the bottom left corner and the width/height.

move :: Position -> Vector -> Position
move (x, y) (dx, dy) = (x + dx, y + dy)

class HasPosition a where
  pos :: a -> Position

  -- Given an object that has a position and a hitBoxSize, return its hitbox
  hitbox :: a -> Square
  hitbox a = Square {bottomLeft = squarePos, width = size}
    where
      (x, y) = pos a
      size = hitboxSize a
      squarePos = (x - (size / 2), y - (size / 2))
                                    
  hitboxSize :: a -> Float

  -- The object is out of bounds when all corners are outside the screen.
  outOfBounds :: a -> Bool
  outOfBounds a = all (< screenMinX) xCoordinates || all (> screenMaxX) xCoordinates || all (< screenMinY) yCoordinates || all (> screenMaxY) yCoordinates
                    where cornersObject = corners (hitbox a)
                          (xCoordinates, yCoordinates) = unzip cornersObject

  -- Given two objects, return these arguments if they've hit each other. 
  hit :: HasPosition b => a -> b -> Maybe (a, b)
  hit a b | hitboxesIntersect a b = Just (a, b)
          | otherwise = Nothing

  -- Given two objects with a position, return if their hitboxes intersect or not.
  hitboxesIntersect ::  HasPosition b => a -> b -> Bool
  hitboxesIntersect a b = any (`pointInHitbox` h1) (corners h2) || any (`pointInHitbox` h2) (corners h1)
      where
        (h1, h2) = (hitbox a, hitbox b)

-- Return a list with all the corners of a square.  
corners :: Square -> [Position]
corners (Square bottomLeft@(bottomLeftX, bottomLeftY) width) = [bottomLeft, (bottomLeftX + width, bottomLeftY), (bottomLeftX, bottomLeftY + width), (bottomLeftX+width, bottomLeftY + width)]

-- Given a position an a hitbox, return whether the position is in that hitbox or not.
pointInHitbox :: Position -> Square -> Bool
pointInHitbox (x, y) (Square (bottomLeftX,bottomLeftY) width) = and [bottomLeftX <= x, x <= bottomLeftX + width, bottomLeftY <= y, y <= bottomLeftY + width]

-- Enumeration of all directions along the x and y axes
data Direction = ToTop | ToBottom | ToLeft | ToRight
