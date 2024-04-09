import Graphics.Gloss

import World (mazeToPicture,handleInput,initializeWorld)
import Dijkstra (calculateMinSteps)
import Maze (generateLeaves)
import Map (mazeMap)

main :: IO ()
main = do
  let initialMaze = mazeMap

  leaves <- generateLeaves initialMaze
  newWorld <- initializeWorld leaves
  minSteps <- calculateMinSteps newWorld leaves initialMaze

  play
    (InWindow "Belesminha" (600, 600) (0, 0))
    black
    60
    newWorld
    (mazeToPicture minSteps)
    handleInput
    (\time nothing -> nothing)
