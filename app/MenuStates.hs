    module MenuStates where

    import Graphics.Gloss.Interface.IO.Game
    import MenuButtons

    newtype MenuSelectionState = MenuSelectionState { selectedOption :: Int }
    data GameState = MainMenu | Game | Score | Instructions deriving Eq

    data State = State
        { gameState :: GameState
        , menuState :: MenuSelectionState
        }

    initialState :: State
    initialState = State MainMenu (MenuSelectionState 0)

    render :: State -> Picture
    render (State MainMenu menuState) = renderMenu menuState
    render (State Game _) = renderGame

    renderMenu :: MenuSelectionState -> Picture
    renderMenu state = pictures
        [  translate 0 50 (
                if selectedOption state == 0 then newGame True
                else newGame False
            )
        , translate 0 0 (
                if selectedOption state == 1 then instructions True
                else instructions False
            )
        , translate 0 (-50) (
                if selectedOption state == 2 then score True
                else score False
            )
        ,  translate 0 (-100) (
                if selectedOption state == 3 then quit True
                else quit False
            )
        ]

    renderGame :: Picture
    renderGame = color green (rectangleSolid 400 400)

    handleEvent :: Event -> State -> State
    handleEvent (EventKey (SpecialKey KeyUp) Down _ _) state =
        state { menuState = (menuState state) { selectedOption = (selectedOption (menuState state) - 1) `mod` 4 } }
    handleEvent (EventKey (SpecialKey KeyDown) Down _ _) state =
        state { menuState = (menuState state) { selectedOption = (selectedOption (menuState state) + 1) `mod` 4 } }
    -- Evento de pressionar enter no novo jogo (TODO: Atualizar o state para o jogo)
    handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) state
        | selectedOption (menuState state) == 0 = state { gameState = Game }
        | otherwise = state
    handleEvent _ state = state


    update :: Float -> State -> State
    update _ state = state