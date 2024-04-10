{-# LANGUAGE GADTs #-}

module World where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
    (Event(EventKey),Key(SpecialKey),KeyState(Down,Up),SpecialKey(KeyRight,KeyUp,KeyDown,KeyLeft))

import Map (mazeMap,cellSize,cellToPicture,Cell(..))
import Maze (Maze,Coord,updateMaze,Coord,Directions(..),goToNeighbor)

initializeWorld :: [Coord] -> IO World
initializeWorld leavesList = do
    let initialMaze = mazeMap
        leaves = leavesList
    let mazeWithLeaves = foldl (\mz (x,y) -> updateMaze mz (x,y) Leaf) initialMaze leaves
        newWorld = World {
            worldMap  = mazeWithLeaves,
            endPos    = (23, 17),
            startPos  = (1, 1),
            playerPos = (1, 1),
            moveCount = 0
        }
    return newWorld

data World where
  World :: { worldMap :: Maze, 
        endPos :: Coord, 
        startPos :: Coord, 
        playerPos :: Coord,
        moveCount :: Int } -> World

mazeToPicture :: [Coord] -> World -> Picture
mazeToPicture minSteps world =
    let maze = worldMap world
        xa =  map(\(x, y) -> y) minSteps
        yb = map(\(x, y) -> 24-x) minSteps
        reversedMaze = reverse maze
        minStepsPictures = pictures [translate (fromIntegral x * cellSize) (fromIntegral y * cellSize) (color (makeColor 0 0 1 0.2) $ rectangleSolid cellSize cellSize) | (x, y) <- zip xa yb]
    in translate (-240) (-225) . pictures $
                                    [ translate (x * cellSize) (y * cellSize) (cellToPicture cell) | (y,  row) <- zip [0..] reversedMaze , (x, cell) <- zip [0..] row ] ++ [minStepsPictures] ++ [drawStepCount (moveCount world)]

drawStepCount :: Int -> Picture
drawStepCount n = Translate (0) (-40) $ Scale 0.3 0.3 $ Color white $ Text $ "STEPS: " ++ show n

changeDirection :: Event -> Directions
changeDirection (EventKey (SpecialKey KeyDown) Down _ _)  = ToDown
changeDirection (EventKey (SpecialKey KeyUp) Down _ _)    = ToUp
changeDirection (EventKey (SpecialKey KeyLeft) Down _ _)  = ToLeft
changeDirection (EventKey (SpecialKey KeyRight) Down _ _) = ToRight
changeDirection _                                         = None

incrementStep :: Coord -> Coord -> Int
incrementStep (x1,y1) (x2,y2)
    | x1 == x2 && y1 == y2 = 0
    | otherwise = 1

handleInput :: Event -> World -> World
handleInput ev world =
    World { worldMap = newMap', playerPos = newPos, moveCount = stepsTaken+increase }
    where
        map = worldMap world
        plPos = playerPos world
        stepsTaken = moveCount world
        newMap = updateMaze map plPos Path
        
        dir = changeDirection ev
        newPos = goToNeighbor map plPos dir
        newMap' = updateMaze newMap newPos Start
        increase = incrementStep plPos newPos