module Model.General (
  Position,
  Vector,
  Score,
  move,
  outOfBounds,
  HasPosition,
  pos
) where

import Model.Parameters (screenMin, screenMax)

type Position = (Float, Float)
type Vector   = (Float, Float) -- x and y component
type Score    = Int

move :: Position -> Vector -> Position
move (x, y) (dx, dy) = (x + dx, y + dy)

outOfBounds :: Position -> Bool
outOfBounds (x, y) = x < screenMin || x > screenMax || y < screenMin || y > screenMax

class HasPosition a where
  pos :: a -> Position
