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

cellSize :: Float
cellSize = 20

cellToPicture :: Cell -> Picture
cellToPicture Wall = color black $ rectangleSolid cellSize cellSize
cellToPicture Path = color white $ rectangleSolid cellSize cellSize
cellToPicture Start = color green $ rectangleSolid cellSize cellSize
cellToPicture End = color red $ rectangleSolid cellSize cellSize
cellToPicture Leaf = color orange $ rectangleSolid cellSize cellSize
cellToPicture MinStep = color (makeColor 0.5 0.5 0.5 1) $ rectangleSolid cellSize cellSize

sampleWorld :: Maze -> World
sampleWorld maze = World {worldMap = maze}

data World = World {worldMap :: Maze}

main :: IO ()
main = do
  let initialMaze = mazeMap

  leaves <- generateLeaves initialMaze
  newWorld <- initializeWorld leaves
  minSteps <- calculateMinSteps newWorld leaves initialMaze

  play
    (InWindow "Belesminha" (600, 600) (0, 0))
    white
    60
    newWorld
    (mazeToPicture minSteps)
    handleInput
    (\time nothing -> nothing)
