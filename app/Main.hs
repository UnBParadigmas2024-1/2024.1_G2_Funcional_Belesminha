import Graphics.Gloss

import World (mazeToPicture,handleInput,initializeWorld,updateWorld)
import Dijkstra (calculateMinSteps)
import Maze (generateLeaves)
import Map (mazeMap)
import Graphics.Gloss.Interface.IO.Game (playIO)
import MenuSetUp (menuWindow, menuBackgroundColor, menuDrawing) 

windowDisplay :: Display
windowDisplay = InWindow "Belesminha: Menu principal" (600, 600) (100, 100)

main :: IO ()
main = do
  let initialMaze = mazeMap

  leaves <- generateLeaves initialMaze
  newWorld <- initializeWorld leaves
  minSteps <- calculateMinSteps newWorld leaves initialMaze
  
  -- playIO 
  --   (InWindow "Belesminha: Menu principal" (600, 600) (100, 100))
  --   black
  --   60
  --   newWorld
  --   (mazeToPicture minSteps)
  --   handleInput
  --   updateWorld
    -- (InWindow "Belesminha: Menu principal" (600, 600) (100, 100))
     
  
  play
    (InWindow "Belesminha: Jogando" (600, 600) (0, 0))
    black
    60
    newWorld
    (mazeToPicture minSteps)
    handleInput
    updateWorld
