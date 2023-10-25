module View where

import Graphics.Gloss

-- Show a screen using Gloss with a circle.
window :: Display
window = InWindow "Text" (500, 500) (10, 10)

picture :: Picture
picture = circle 100

main :: IO ()
main = display window white picture 