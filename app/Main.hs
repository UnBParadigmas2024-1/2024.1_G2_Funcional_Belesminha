import World ( sampleWorld, mazeToPicture, handleInput, initializeWorld )
import Maze ( generateLeaves, updateMaze )
import Map ( mazeMap, Cell(Leaf) )
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact (Event)
import Graphics.Gloss.Interface.IO.Game (Event(..), KeyState(..), Key(..))
import Map (mazeMap, Cell(..))
import Maze (Maze, Coord, generateLeaves, updateMaze)
import Dijkstra (calculateMinSteps)

import Control.Monad.IO.Class (liftIO)

endPos :: Coord
endPos = (23, 17)

startPos :: Coord
startPos = (1, 1)


cellSize :: Float
cellSize = 20

cellToPicture :: Cell -> Picture
cellToPicture Wall = color black $ rectangleSolid cellSize cellSize
cellToPicture Path = color white $ rectangleSolid cellSize cellSize
cellToPicture Start = color green $ rectangleSolid cellSize cellSize
cellToPicture End = color red $ rectangleSolid cellSize cellSize
cellToPicture Leaf = color orange $ rectangleSolid cellSize cellSize

sampleWorld :: Maze -> World
sampleWorld maze = World {worldMap = maze}

data World = World {worldMap :: Maze}

main :: IO ()
main = do
  let initialMaze = mazeMap
  leaves <- generateLeaves initialMaze
  minSteps <- calculateMinSteps startPos endPos leaves mazeMap
  -- viewMaze initialMaze  -- Exibe o labirinto
  print minSteps        -- Imprime o número mínimo de passos
  print leaves
  newWorld <- initializeWorld
  let mazeWithLeaves = foldl (\mz (x, y) -> updateMaze mz (x, y) Leaf) initialMaze leaves
  play
    (InWindow "Belesminha" (600, 600) (0, 0))
    white
    60
    newWorld
    mazeToPicture
    handleInput
    (\time nothing -> nothing)
