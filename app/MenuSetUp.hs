module MenuSetUp where

import Graphics.Gloss

data MenuEntry = NewGame | Instructions | Score | Quit deriving (Eq)
data MenuEntryState = Selected | Idle deriving (Eq)

menuWindow :: Display
menuWindow = InWindow "Belesminha: Menu Principal" (600, 600) (0, 0)

menuBackgroundColor :: Color
menuBackgroundColor = white

menuDrawing :: Picture
menuDrawing = Circle 80

newGame :: Picture
newGame = Text "Novo Jogo"

instructions :: Picture
instructions = Text "Instruções"

score :: Picture
score = Text "Placar"

selectedEntryBox :: Picture
selectedEntryBox = color blue $ rectangleSolid 40.0 40.0

quit :: Picture
quit = Text "Sair"

menuEntry :: MenuEntry -> MenuEntryState  -> Picture
menuEntry NewGame Selected = Pictures [color white newGame, selectedEntryBox]
menuEntry NewGame Idle = newGame


