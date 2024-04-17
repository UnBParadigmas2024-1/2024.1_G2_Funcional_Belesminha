module MenuButtons where

import Graphics.Gloss

data MenuEntry = NewGame | Instructions | Score | Quit deriving (Eq)
data MenuEntryState = Selected | Idle deriving (Eq)

instance Show MenuEntryState where
    show Selected = "Selected"
    show Idle = "Idle"

instance Show MenuEntry where
    show NewGame = "newgame"
    show Instructions = "instructions"
    show Score = "score"
    show Quit = "quit"

menuWindow :: Display
menuWindow = InWindow "Belesminha: Menu Principal" (600, 600) (0, 0)

menuBackgroundColor :: Color
menuBackgroundColor = white

menuDrawing :: Picture
menuDrawing = Circle 80

newGame :: Picture
newGame = translate (-85) (-10) $ textWithSize 20 "Novo Jogo"

instructions :: Picture
instructions = Text "Instruções"

score :: Picture
score = Text "Placar"

selectedEntryBox :: Picture
selectedEntryBox = color blue $ rectangleSolid 500.0 100.0

quit :: Picture
quit = Text "Sair"

textWithSize :: Int -> String -> Picture
textWithSize fontSize text = Scale 0.3 0.3 $ Text text

menuEntry :: MenuEntry -> MenuEntryState  -> Picture
menuEntry NewGame Selected = Pictures [selectedEntryBox, color white newGame]
menuEntry NewGame Idle = newGame


