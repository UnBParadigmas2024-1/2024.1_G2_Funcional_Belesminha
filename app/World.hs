module World where

import Graphics.Gloss.Interface.IO.Game (Event(..), KeyState(..), Key(..))
import Map ( mazeMap, Cell(..) )
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact (Event)
import Maze ( Maze, generateLeaves, updateMaze )
import System.IO.Unsafe (unsafePerformIO)


cellSize :: Float
cellSize = 20

cellToPicture :: Cell -> Picture
cellToPicture Wall = color black $ rectangleSolid cellSize cellSize
cellToPicture Path = color white $ rectangleSolid cellSize cellSize
cellToPicture Start = color green $ rectangleSolid cellSize cellSize
cellToPicture End = color red $ rectangleSolid cellSize cellSize
cellToPicture Leaf = color orange $ rectangleSolid cellSize cellSize

initializeWorld :: IO World
initializeWorld = do
    let initialMaze = mazeMap
    leaves <- generateLeaves initialMaze
    let mazeWithLeaves = foldl (\mz (x,y) -> updateMaze mz (x,y) Leaf) initialMaze leaves
        newWorld = World {
            worldMap = mazeWithLeaves
        }
    return newWorld

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
handleInput (EventKey (Char 'r') Down _ _) _ = unsafePerformIO initializeWorld
handleInput _ world = world