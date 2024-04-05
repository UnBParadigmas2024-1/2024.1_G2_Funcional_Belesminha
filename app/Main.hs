import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact (Event)
import Graphics.Gloss.Interface.IO.Game (Event(..), KeyState(..), Key(..))
import Map ( mazeMap, Cell(..) )
import Maze ( Maze, generateLeaves, updateMaze )

import Control.Monad.IO.Class (liftIO)


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

data World where
  World :: {worldMap :: Maze} -> World

mazeToPicture :: World -> Picture
mazeToPicture world =
    let maze = worldMap world
        reversedMaze = reverse maze
    in translate (-240) (-225) . pictures $
    [ translate (x * cellSize) (y * cellSize) (cellToPicture cell)
    | (y,  row) <- zip [0..] reversedMaze
    , (x, cell) <- zip [0..] row
    ]

handleInput :: Event -> World -> World
handleInput (EventKey (Char 'r') Down _ _) world =
    let maze = worldMap world
    in world { worldMap = updateMaze maze (0,0) Path }
handleInput _ world = world


main :: IO ()
main = do
    let initialMaze = mazeMap
    leaves <- generateLeaves initialMaze
    let mazeWithLeaves = foldl (\mz (x,y) -> updateMaze mz (x,y) Leaf) initialMaze leaves
    play
        (InWindow "Belesminha" (600,600) (0,0))
        white
        30
        (sampleWorld mazeWithLeaves)
        mazeToPicture
        handleInput
        (\_ nothing -> nothing)