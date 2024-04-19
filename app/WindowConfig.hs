module WindowConfig where

import Graphics.Gloss 

data CurrentWindow = MenuWindow | GameWindow | ScoreWindow | InstructionsWindow deriving(Eq)
instance Show CurrentWindow where
    show MenuWindow = "Menu"
    show GameWindow = "Game"
    show ScoreWindow = "Score"
    show InstructionsWindow = "Instructions"

windowDisplay :: CurrentWindow -> Display
windowDisplay MenuWindow = InWindow "Belesminha: Menu principal" (1000, 1000) (0, 0)
windowDisplay GameWindow = InWindow "Belesminha: Jogando" (1000, 1000) (0, 0)
windowDisplay ScoreWindow = InWindow "Belesminha: Placar" (1000, 1000) (0, 0)
windowDisplay InstructionsWindow = InWindow "Belesminha: Instrucoes" (1000, 1000) (0, 0)

