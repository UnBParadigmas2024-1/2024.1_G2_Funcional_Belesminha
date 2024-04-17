import Graphics.Gloss

import World (mazeToPicture,handleInput,initializeWorld,updateWorld,World(..))
import Dijkstra (calculateMinSteps)
import Maze (generateLeaves)
import Map (mazeMap)

main :: IO ()
main = do
  let initialMaze = mazeMap

  leaves <- generateLeaves initialMaze
  newWorld' <- initializeWorld leaves
  minSteps <- calculateMinSteps newWorld' leaves initialMaze
  
  let newWorld = newWorld' { maxSteps = length minSteps }
  play
    (InWindow "Belesminha" (1000, 1000) (0, 0))
    black
    60
    newWorld
    (mazeToPicture minSteps)
    handleInput
    updateWorld
