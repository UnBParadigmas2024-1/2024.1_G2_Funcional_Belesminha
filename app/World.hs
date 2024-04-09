{-# LANGUAGE GADTs #-}

module World where

import Graphics.Gloss.Interface.IO.Game (Event(..), KeyState(..), Key(..))
import Map ( mazeMap, Cell(..) )
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact (Event)
import Graphics.Gloss.Data.Color (makeColor, blue)
import qualified Data.Map.Strict as Map
import Graphics.Gloss.Interface.Pure.Game
    ( Event(EventKey),
      Key(SpecialKey),
      KeyState(Down, Up),
      SpecialKey(KeyRight, KeyUp, KeyDown, KeyLeft) )
import Maze ( Maze, Coord, generateLeaves, updateMaze, startCoord, incrementS, incrementW, incrementA, incrementD, Coord, Directions(..), goToNeighbor )
import System.IO.Unsafe (unsafePerformIO)

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
            startPos = (1, 1),
            playerPos = (1, 1)
        }
    return newWorld

data World where

  World :: {worldMap :: Maze, endPos :: Coord, startPos :: Coord, playerPos :: Coord} -> World

mazeToPicture :: [Coord] -> World -> Picture
mazeToPicture minSteps world =
    let maze = worldMap world
        xa =  map(\(x, y) -> y) minSteps
        yb = map(\(x, y) -> 24-x) minSteps
        reversedMaze = reverse maze
        minStepsPictures = pictures [translate (fromIntegral x * cellSize) (fromIntegral y * cellSize) (color (makeColor 0 0 1 0.2) $ rectangleSolid cellSize cellSize) | (x, y) <- zip xa yb]
    in translate (-240) (-225) . pictures $
                                    [ translate (x * cellSize) (y * cellSize) (cellToPicture cell) | (y,  row) <- zip [0..] reversedMaze , (x, cell) <- zip [0..] row ] ++ [minStepsPictures]

-- handleInput :: Event -> World -> World
-- handleInput (EventKey (Char 'r') Down _ _) _ = unsafePerformIO initializeWorld
-- handleInput (EventKey (Char 'q') Down _ _) _ = updateMaze mazeMap incrementS Start

handleInput :: Event -> World -> World
handleInput (EventKey (SpecialKey KeyDown) Down _ _)  world =
    World { worldMap = updateMaze (updateMaze (worldMap world) (playerPos world) Path) 
                        (goToNeighbor (worldMap world) (playerPos world) ToDown) Start,
                        playerPos = (goToNeighbor (worldMap world) (playerPos world) ToDown) }
handleInput (EventKey (SpecialKey KeyUp) Down _ _)  world =
    World { worldMap = updateMaze (updateMaze (worldMap world) (playerPos world) Path) 
                        (goToNeighbor (worldMap world) (playerPos world) ToUp) Start,
                        playerPos = (goToNeighbor (worldMap world) (playerPos world) ToUp) }
handleInput (EventKey (SpecialKey KeyLeft) Down _ _)  world =
    World { worldMap = updateMaze (updateMaze (worldMap world) (playerPos world) Path) 
                        (goToNeighbor (worldMap world) (playerPos world) ToLeft) Start,
                        playerPos = (goToNeighbor (worldMap world) (playerPos world) ToLeft) }
handleInput (EventKey (SpecialKey KeyRight) Down _ _)  world =
    World { worldMap = updateMaze (updateMaze (worldMap world) (playerPos world) Path) 
                        (goToNeighbor (worldMap world) (playerPos world) ToRight) Start,
                        playerPos = (goToNeighbor (worldMap world) (playerPos world) ToRight) }
handleInput _ world = world