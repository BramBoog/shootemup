module Model.Movement (
  Position,
  Vector,
  move,
  outOfBounds,
  HasPosition,
  pos
) where

import Model.Parameters (screenMin, screenMax)

type Position = (Float, Float)
type Vector   = (Float, Float) -- x and y component

move :: Position -> Vector -> Position
move (x, y) (dx, dy) = (x + dx, y + dy)

outOfBounds :: Position -> Bool
outOfBounds (x, y) = x < screenMin || x > screenMax || y < screenMin || y > screenMax

class HasPosition a where
  pos :: a -> Position
