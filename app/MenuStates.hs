    module MenuStates where

    import Graphics.Gloss.Interface.IO.Game
    import MenuButtons
    import Map 
    import Maze
    import Dijkstra

    newtype MenuSelectionState = MenuSelectionState { selectedOption :: Int }
    data GameState = MainMenuState | Game | ScoreState | InstructionsState deriving Eq

    data State = State
        { 
            gameState :: GameState,
            menuState :: MenuSelectionState
        }

    -- Check if the gameState in State is Game
    isGameStateGame :: State -> Bool
    isGameStateGame state = gameState state == Game

    initialState :: State
    initialState = State MainMenuState (MenuSelectionState 0)


    render :: State -> Picture
    render (State MainMenuState menuState) = renderMenu menuState
    -- render (State Game _) = calculateMinSteps (initializeWorld (generateLeaves mazeMap))

    renderMenu :: MenuSelectionState -> Picture
    renderMenu state = pictures
        [ translate 0 200 (
            pictures [titleBGBack, titleBGFront, translate (-130) (-8) $ mainTitle "Belesminha: O Jogo"]
        ) 
        , translate 0 50 (
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
        ,  translate (-160) (-280) (
                quitText "Pressione ESC para sair"
            )
        ]

    renderGame :: Picture
    renderGame = color green (rectangleSolid 400 400)

    handleEvent :: Event -> State -> State
    handleEvent (EventKey (SpecialKey KeyUp) Down _ _) state =
        state { menuState = (menuState state) { selectedOption = (selectedOption (menuState state) - 1) `mod` 4 } }
    handleEvent (EventKey (SpecialKey KeyDown) Down _ _) state =
        state { menuState = (menuState state) { selectedOption = (selectedOption (menuState state) + 1) `mod` 4 } }
    handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) state
        | selectedOption (menuState state) == 0 = state { gameState = Game }  
        | otherwise = state
    handleEvent _ state = state


    update :: Float -> State -> State
    update _ state = state