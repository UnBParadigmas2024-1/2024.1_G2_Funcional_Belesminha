module MenuSetUp where

import Graphics.Gloss

menuWindow :: Display
menuWindow = InWindow "Belesminha: Menu Principal" (600, 600) (0, 0)

menuBackgroundColor :: Color
menuBackgroundColor = black

menuDrawing :: Picture
menuDrawing = circle 80
