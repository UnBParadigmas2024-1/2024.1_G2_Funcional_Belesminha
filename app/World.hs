module World where

import Graphics.Gloss.Interface.IO.Game (Event(..), KeyState(..), Key(..))
import Map ( mazeMap, Cell(..) )
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact (Event)
import Maze ( Maze, Coord, generateLeaves, updateMaze )
import System.IO.Unsafe (unsafePerformIO)
import Graphics.Gloss.Data.Color (makeColor, blue)

cellSize :: Float
cellSize = 20

cellToPicture :: Cell -> Picture
cellToPicture Wall = color black $ rectangleSolid cellSize cellSize
cellToPicture Path = color white $ rectangleSolid cellSize cellSize
cellToPicture Start = color green $ rectangleSolid cellSize cellSize
cellToPicture End = color red $ rectangleSolid cellSize cellSize
cellToPicture Leaf = color orange $ rectangleSolid cellSize cellSize

initializeWorld :: [Coord] -> IO World
initializeWorld leavesList = do
    let initialMaze = mazeMap
        leaves = leavesList
    let mazeWithLeaves = foldl (\mz (x,y) -> updateMaze mz (x,y) Leaf) initialMaze leaves
        newWorld = World {
            worldMap = mazeWithLeaves,
            endPos = (23, 17),
            startPos = (1, 1)
        }
    return newWorld

sampleWorld :: Maze -> World
sampleWorld maze = World {worldMap = maze}

data World where
  World :: {worldMap :: Maze, endPos :: Coord, startPos :: Coord} -> World

mazeToPicture :: [Coord] -> World -> Picture
mazeToPicture minSteps world =
    let maze = worldMap world
        xa =  map(\(x, y) -> y) minSteps
        yb = map(\(x, y) -> 24-x) minSteps
        reversedMaze = reverse maze
        minStepsPictures = pictures [translate (fromIntegral x * cellSize) (fromIntegral y * cellSize) (color (makeColor 0 0 1 0.2) $ rectangleSolid cellSize cellSize) | (x, y) <- zip xa yb]
    in translate (-240) (-225) . pictures $
                                    [ translate (x * cellSize) (y * cellSize) (cellToPicture cell) | (y,  row) <- zip [0..] reversedMaze , (x, cell) <- zip [0..] row ] ++ [minStepsPictures]

handleInput :: Event -> World -> World
handleInput (EventKey (Char 'q') Down _ _) _ = error "Bye!"
handleInput _ world = world