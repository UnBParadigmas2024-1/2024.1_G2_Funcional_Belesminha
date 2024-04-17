import Graphics.Gloss

import World (mazeToPicture,handleInput,initializeWorld,updateWorld)
import Dijkstra (calculateMinSteps)
import Maze (generateLeaves)
import Map (mazeMap)
import Graphics.Gloss.Interface.IO.Game (playIO)
import MenuButtons (menuWindow, menuBackgroundColor, menuDrawing, MenuEntry (NewGame), MenuEntryState (Selected)) 
import Graphics.Gloss.Interface.IO.Display (displayIO)
import MenuStates
import WindowConfig

main :: IO ()
main = do
  let initialMaze = mazeMap

  leaves <- generateLeaves initialMaze
  newWorld <- initializeWorld leaves
  minSteps <- calculateMinSteps newWorld leaves initialMaze

  let menuPlay = play
        (windowDisplay MenuWindow)
        black
        60
        initialState
        (renderMenu . menuState)
        handleEvent
        update

  let gamePlay = play
        (windowDisplay GameWindow)
        black
        60
        newWorld
        (mazeToPicture minSteps)
        handleInput
        updateWorld

  case gameState initialState of
    MainMenu -> menuPlay
    Game -> gamePlay
