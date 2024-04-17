import Graphics.Gloss

import World (mazeToPicture,handleInput,initializeWorld,updateWorld)
import Dijkstra (calculateMinSteps)
import Maze (generateLeaves)
import Map (mazeMap)
import Graphics.Gloss.Interface.IO.Game (playIO)
import MenuButtons (menuWindow, menuBackgroundColor, menuDrawing, MenuEntry (NewGame), MenuEntryState (Selected)) 
import Graphics.Gloss.Interface.IO.Display (displayIO)
import MenuStates

windowDisplay :: Display
windowDisplay = InWindow "Belesminha: Menu principal" (600, 600) (100, 100)

main :: IO ()
main = do
  let initialMaze = mazeMap

  leaves <- generateLeaves initialMaze
  newWorld <- initializeWorld leaves
  minSteps <- calculateMinSteps newWorld leaves initialMaze

  play
    windowDisplay
    black
    60
    initialState
    renderMenu
    handleEvent
    update
  


  -- display menuWindow menuBackgroundColor (menuEntry NewGame Selected)

  -- displayIO menuWindow menuBackgroundColor menuEntries controllerSetRedraw 

  -- playIO 
  --   (InWindow "Belesminha: Menu principal" (600, 600) (100, 100))
  --   black
  --   60
  --   newWorld
  --   (mazeToPicture minSteps)
  --   handleInput
  --   updateWorld
    -- (InWindow "Belesminha: Menu principal" (600, 600) (100, 100))
     
  
  -- play
  --   (InWindow "Belesminha: Jogando" (600, 600) (0, 0))
  --   black
  --   60
  --   newWorld
  --   (mazeToPicture minSteps)
  --   handleInput
  --   updateWorld
