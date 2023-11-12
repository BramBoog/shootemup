module Model.Movement (
  Position,
  Vector,
  move,
  HasPosition (pos, hit, outOfBounds),
  Direction (ToTop, ToBottom, ToRight, ToLeft)
) where

import Model.Parameters (screenMinX, screenMaxX, screenMinY, screenMaxY)

type Position = (Float, Float)
type Vector   = (Float, Float) -- x and y component

move :: Position -> Vector -> Position
move (x, y) (dx, dy) = (x + dx, y + dy)

class HasPosition a where
  pos :: a -> Position

  outOfBounds :: a -> Bool
  outOfBounds a = let (x, y) = pos a
                   in x < screenMinX || x > screenMaxX || y < screenMinY || y > screenMaxY
  
  -- Given two objects, return these arguments if they've hit each other. 
  hit :: HasPosition b => a -> b -> Maybe (a, b)
  hit a b | pos a == pos b = Just (a, b) -- Change to this later: if the position is approximately equal.
          | otherwise = Nothing

-- Enumeration of all directions along the x and y axes
data Direction = ToTop | ToBottom | ToLeft | ToRight
