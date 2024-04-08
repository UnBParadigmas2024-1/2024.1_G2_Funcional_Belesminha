module World where

import Graphics.Gloss.Interface.IO.Game (Event(..), KeyState(..), Key(..))
import Map ( mazeMap, Cell(..) )
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact (Event)
import qualified Data.Map.Strict as Map
import Graphics.Gloss.Interface.Pure.Game
    ( Event(EventKey),
      Key(SpecialKey),
      KeyState(Down, Up),
      SpecialKey(KeyRight, KeyUp, KeyDown, KeyLeft) )
import Maze ( Maze, generateLeaves, updateMaze, startCoord, incrementS, incrementW, incrementA, incrementD, Coord )
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
            worldMap = mazeWithLeaves,
            currentStartCoord = (1, 1)
        }
    return newWorld

sampleWorld :: Maze -> World
sampleWorld maze = World {worldMap = maze}

data World where
  World :: {worldMap :: Maze, currentStartCoord :: Coord } -> World

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
-- handleInput (EventKey (Char 'q') Down _ _) _ = updateMaze mazeMap incrementS Start
handleInput (EventKey (SpecialKey KeyUp) Down _ _)  world =
  let currentMaze = mazeMap
      currentStartCoord = startCoord
      newMaze = updateMaze currentMaze (incrementW currentStartCoord) Start
  in World { worldMap = newMaze, currentStartCoord = currentStartCoord }

-- handleInput (EventKey (SpecialKey KeyDown) Down _ _)  world =
--   let currentMaze = mazeMap
--       currentStartCoord = currentStartCoord
--       newMaze = updateMaze currentMaze (incrementS currentStartCoord) Start
--   in World { worldMap = newMaze, currentStartCoord = currentStartCoord }

handleInput (EventKey (SpecialKey KeyDown) Down _ _)  world =
  let currentMaze = mazeMap
      currentStartPos = currentStartCoord world
      newMaze = updateMaze currentMaze (incrementS currentStartPos) Start
  in World { worldMap = newMaze, currentStartCoord = currentStartPos }

handleInput (EventKey (SpecialKey KeyRight) Down _ _)  world =
  let currentMaze = mazeMap
      currentStartCoord = startCoord
      newMaze = updateMaze currentMaze (incrementD currentStartCoord) Start
  in World { worldMap = newMaze, currentStartCoord = currentStartCoord }
-- handleInput (EventKey (SpecialKey KeyLeft) Down _ _) (World world) = World (updateMaze mazeMap (incrementA startCoord) Start)
-- handleInput (EventKey (SpecialKey KeyDown) Down _ _) (World world) = World (updateMaze mazeMap (incrementS startCoord) Start)
-- handleInput (EventKey (SpecialKey KeyRight) Down _ _) (World world) = World (updateMaze mazeMap (incrementD startCoord) Start)
handleInput _ world = world