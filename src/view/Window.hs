module View.Window where

import Graphics.Gloss
import GHC.Float (float2Int)
import Model.Parameters (screenSizeX, screenSizeY)

window :: Display
window = InWindow "Shoot 'Em Up" (2 * float2Int screenSizeX, 2 * float2Int screenSizeY) (0, 0)

