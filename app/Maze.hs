
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use elemIndex" #-}
{-# LANGUAGE BlockArguments #-}

module Maze(Maze,Coord,updateMaze,generateLeaves,Directions(..),goToNeighbor) where

import System.Random ( randomRIO )
import Control.Monad (replicateM)
import Data.List (findIndex)

import Map(mazeMap,Cell(..))

type Maze = [[Cell]]
type Coord = (Int,Int)

data Directions =
    ToUp
    | ToDown
    | ToLeft
    | ToRight
    | None
    deriving (Eq)

viewMaze :: Maze -> IO ()
viewMaze = mapM_ (putStrLn . concatMap showCell)
    where
        showCell Wall   = "W "
        showCell Path   = "P "
        showCell Start  = "S "
        showCell End    = "E "
        showCell Leaf   = "L "

updateMaze :: Maze -> Coord -> Cell -> Maze
updateMaze maze (x,y) cll = a ++ [l ++ cll:r] ++ b
    where
        (a,row:b)  = splitAt x maze
        (l,col:r)  = splitAt y row

numberOfLeaves :: Int
numberOfLeaves = 5

generateLeaf :: Maze -> IO Coord
generateLeaf maze = do
    let (x:xs)  = maze
        numRows = length maze
        numCols = length x
    x <- randomRIO (1, numRows - 1)
    y <- randomRIO (1, numCols - 1)
    if maze !! x !! y == Path
        then return (x, y)
        else generateLeaf maze

generateLeaves :: Maze -> IO [Coord]
generateLeaves maze = Control.Monad.replicateM numberOfLeaves (generateLeaf maze)

goToNeighbor :: Maze -> Coord -> Directions -> Coord
goToNeighbor maze (x,y) dir =
    let (dx,dy) = directionOffset dir
        rows = (length maze) - 1
        cols = (length (maze !! 0)) - 1
        new_x = if x+dx < 0 then 0 else if x+dx > rows then rows else x+dx
        new_y = if y+dy < 0 then 0 else if y+dy > cols then cols else y+dy
        newCoord = if maze !! new_x !! new_y == Wall then (x, y) else (new_x, new_y)
    in newCoord

directionOffset :: Directions -> Coord
directionOffset dir =
    case dir of
        ToUp    -> (-1,0)
        ToDown  -> (1, 0)
        ToRight -> (0, 1)
        ToLeft  -> (0,-1)
        None    -> (0, 0)
