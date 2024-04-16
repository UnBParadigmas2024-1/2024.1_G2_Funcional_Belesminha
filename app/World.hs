{-# LANGUAGE GADTs #-}

module World where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
    (Event(EventKey),Key(SpecialKey),KeyState(Down,Up),SpecialKey(KeyRight,KeyUp,KeyDown,KeyLeft))

import Map (mazeMap,cellSize,cellToPicture,Cell(..))
import Maze (Maze,Coord,updateMaze,Coord,Directions(..),goToNeighbor)
import Data.Foldable (Foldable(length))

data PlayState = Playing | GameOver | GameWon deriving (Eq)

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
            moveCount = 0,
            playingState = Playing,
            listOfCurrentPlayerPositions = [(1, 1)],
            maxSteps = 0,
            leafCount = 0
        }
    return newWorld

data World where
  World :: { worldMap :: Maze, 
        endPos :: Coord, 
        startPos :: Coord, 
        playerPos :: Coord,
        moveCount :: Int,
        playingState :: PlayState,
        listOfCurrentPlayerPositions :: [Coord],
        maxSteps :: Int,
        leafCount :: Int
        } -> World

updateWorld :: Float -> World -> World
updateWorld time world 
    | maxStepsReached = world { playingState = GameOver }  -- Verifica se o número máximo de passos foi atingido e atualiza o estado do jogo para GameOver
    | playerPosition == endPosition = world { playingState = GameWon }
    | otherwise = world
    where
        playerPosition = playerPos world
        endPosition = endPos world
        maxStepsReached = moveCount world >= maxSteps world

drawGameOverText :: Picture
drawGameOverText = Translate  100 (-60) $ Scale 0.4 0.4 $ Color white $ Text "GAME OVER"


mazeToPicture :: [Coord] -> World -> Picture
mazeToPicture minSteps world =
    let maze = worldMap world 
        xa =  map(\(x, y) -> y) minSteps
        yb = map(\(x, y) -> 24-x) minSteps
        xp = map(\(x, y) -> y) (listOfCurrentPlayerPositions world)
        yp = map(\(x, y) -> 24-x) (listOfCurrentPlayerPositions world)
        reversedMaze = reverse maze
        minStepsPictures = pictures [translate (fromIntegral x * cellSize) (fromIntegral y * cellSize) (color (makeColor 0 0 1 0.5) $ rectangleSolid cellSize cellSize) | (x, y) <- zip xa yb]
        playerPath = pictures [translate (fromIntegral x * cellSize) (fromIntegral y * cellSize) (color (makeColor 1 0 0 0.1) $ rectangleSolid cellSize cellSize) | (x, y) <- zip xp yp]
    in if playingState world == GameWon
        then translate (-240) (-225) . pictures $
                                    [ translate (x * cellSize) (y * cellSize) (cellToPicture cell) | (y, row) <- zip [0..] reversedMaze , (x, cell) <- zip [0..] row ] ++ [minStepsPictures] ++ [drawStepCount (moveCount world)] ++ [drawMaxSteps (maxSteps world - moveCount world)] ++ [playerPath] ++ [drawLeafCount (leafCount world)]
        else if playingState world == Playing
            then translate (-240) (-225) . pictures $
                                    [ translate (x * cellSize) (y * cellSize) (cellToPicture cell) | (y, row) <- zip [0..] reversedMaze , (x, cell) <- zip [0..] row ] ++ [drawStepCount (moveCount world)]  ++ [drawMaxSteps (maxSteps world - moveCount world)] ++ [playerPath] ++ [drawLeafCount (leafCount world)]
        else if playingState world == GameOver
            then translate (-240) (-225) . pictures $
                                    [ translate (x * cellSize) (y * cellSize) (cellToPicture cell) | (y, row) <- zip [0..] reversedMaze , (x, cell) <- zip [0..] row ] ++ [drawGameOverText] ++ [minStepsPictures] ++ [playerPath]
        else translate (-240) (-225) . pictures $
                                    [ translate (x * cellSize) (y * cellSize) (cellToPicture cell) | (y, row) <- zip [0..] reversedMaze , (x, cell) <- zip [0..] row ]
  

drawStepCount :: Int -> Picture
drawStepCount n = Translate (0) (-40) $ Scale 0.3 0.3 $ Color white $ Text $ "STEPS: " ++ show n 

drawMaxSteps :: Int -> Picture
drawMaxSteps n = Translate (-80) (-40) $ Scale 0.2 0.2 $ Color white $ Text $ "               MAX STEPS: " ++ show n

drawLeafCount :: Int -> Picture
drawLeafCount n = Translate (0) (-100) $ Scale 0.3 0.3 $ Color white $ Text $ "LEAFS: " ++ show n

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

incrementLeafCount :: Maze -> Coord -> Coord -> Int
incrementLeafCount maze (x1,y1) (x2,y2)
    | x1 == x2 && y1 == y2 = 0
    | maze !! x2 !! y2 == Leaf = 1
    | otherwise = 0

handleInput :: Event -> World -> World
handleInput ev world
    | maxSteps world == 0 = world { playingState = GameOver } -- Verifica se o número máximo de passos é 0 e atualiza o estado do jogo para GameOver
    | otherwise =
        world { worldMap = newMap', playerPos = newPos, moveCount = stepsTaken + increase, listOfCurrentPlayerPositions = listOfCurrentPlayerPositions', maxSteps = maxSteps world, leafCount = increaseLeaf + leavesTaken }
    where
        
        map = worldMap world
        plPos = playerPos world
        stepsTaken = moveCount world
        newMap = updateMaze map plPos Path
        
        dir = changeDirection ev
        newPos = goToNeighbor map plPos dir
        newMap' = updateMaze newMap newPos Start
        increase = incrementStep plPos newPos

        leavesTaken = leafCount world
        increaseLeaf = incrementLeafCount newMap plPos newPos

        listOfCurrentPlayerPositions' = if newPos == plPos then listOfCurrentPlayerPositions world else listOfCurrentPlayerPositions world ++ [newPos]
