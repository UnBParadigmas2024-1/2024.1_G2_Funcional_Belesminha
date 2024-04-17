module MenuStates where

import Graphics.Gloss.Interface.IO.Game


newtype MenuState = MenuState { selectedOption :: Int }

initialState :: MenuState
initialState = MenuState 0



renderBasic :: MenuState -> Picture
renderBasic state = pictures
    [ translate 0 50 (
            if selectedOption state == 0 then highlightedEntry "Novo Jogo" 
            else unselectedEntry "Novo Jogo"
        )
    , translate 0 0 (
            if selectedOption state == 1 then highlightedEntry "Instrucoes" 
            else unselectedEntry "Instrucoes"
        )
    , translate 0 (-50) (
            if selectedOption state == 2 then highlightedEntry "Placar" 
            else unselectedEntry "Placar"
        )
    ,  translate 0 (-100) (
            if selectedOption state == 3 then highlightedEntry "Sair" 
            else unselectedEntry "Sair"
        )
    ]
    where
        unselectedEntry :: String -> Picture
        unselectedEntry text = textWithSize 20 text

        highlightedEntry :: String -> Picture
        highlightedEntry text = color red (textWithSize 20 text)

        textWithSize :: Int -> String -> Picture
        textWithSize fontSize text = scale 0.1 0.1 $ Text text

handleEvent :: Event -> MenuState -> MenuState
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) state =
    state { selectedOption = (selectedOption state - 1) `mod` 4 }
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) state =
    state { selectedOption = (selectedOption state + 1) `mod` 4 }
handleEvent _ state = state


update :: Float -> MenuState -> MenuState
update _ = id