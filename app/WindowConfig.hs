module WindowConfig where

import Graphics.Gloss 

data CurrentWindow = MenuWindow | GameWindow | Score | Instructions deriving(Eq)
instance Show CurrentWindow where
    show MenuWindow = "Menu"
    show GameWindow = "Game"
    show Score = "Score"
    show Instructions = "Instructions"

windowDisplay :: CurrentWindow -> Display
windowDisplay MenuWindow = InWindow "Belesminha: Menu principal" (600, 600) (100, 100)
windowDisplay GameWindow = InWindow "Belesminha: Jogando" (600, 600) (100, 100)
windowDisplay Score = InWindow "Belesminha: Placar" (600, 600) (100, 100)
windowDisplay Instructions = InWindow "Belesminha: Instrucoes" (600, 600) (100, 100)