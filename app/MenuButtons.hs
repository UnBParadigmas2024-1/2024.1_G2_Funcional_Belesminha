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

mainTitle :: String -> Picture
mainTitle text = color white (textTitleSize 10 text)

instructionsTitle :: String -> Picture
instructionsTitle text = color white (textTitleSize 10 text)

quitText :: String -> Picture
quitText text = color white (textTitleSize 10 text)

titleBGFront :: Picture
titleBGFront = color blue $ rectangleSolid 360.0 75.0

titleBGBack :: Picture
titleBGBack = color green $ rectangleSolid 400.0 100.0

leafExample :: Picture
leafExample = color red $ rectangleSolid 10.0 10.0

instructionsText :: String -> Picture
instructionsText text = color white $ (textWithSize 10 text)

newGame :: Bool -> Picture
newGame isSelected 
        | isSelected = pictures [selectedEntryBox, color black (translate (-85) (-10) $ textWithSize 10 "Novo Jogo")]
        | otherwise = pictures [color white (translate (-85) (-10) $ textWithSize 10 "Novo Jogo")]

instructions :: Bool -> Picture
instructions isSelected 
        | isSelected = pictures [selectedEntryBox, color black (translate (-85) (-10) $ textWithSize 10 "Instrucoes")]
        | otherwise = pictures [color white (translate (-85) (-10) $ textWithSize 10 "Instrucoes")]

score :: Bool -> Picture
score isSelected 
        | isSelected = pictures [selectedEntryBox, color black (translate (-85) (-10) $ textWithSize 10 "Placar")]
        | otherwise = pictures [color white (translate (-85) (-10) $ textWithSize 10 "Placar")]

quit :: Bool -> Picture
quit isSelected 
        | isSelected = pictures [selectedEntryBox, color black (translate (-85) (-10) $ textWithSize 10 "Sair")]
        | otherwise = pictures [color white (translate (-85) (-10) $ textWithSize 10 "Sair")]

selectedEntryBox :: Picture
selectedEntryBox = color green $ rectangleSolid 500.0 50.0

textWithSize :: Int -> String -> Picture
textWithSize fontSize text = Scale 0.3 0.3 $ Text text

textTitleSize :: Int -> String -> Picture
textTitleSize fontSize text = Scale 0.2 0.2 $ Text text
-- menuEntry :: MenuEntry -> MenuEntryState  -> Picture
-- menuEntry NewGame Selected = Pictures [selectedEntryBox, color white newGame]
-- menuEntry NewGame Idle = newGame


