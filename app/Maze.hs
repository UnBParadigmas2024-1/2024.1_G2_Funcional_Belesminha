{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use elemIndex" #-}
{-# LANGUAGE BlockArguments #-}

module Maze(Maze,Coord,updateMaze,Directions(..),generateLeaves,goToNeighbor) where

import System.Random (randomRIO)
import System.IO.Unsafe (unsafePerformIO)

import Map(Cell(..))

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

generateLeaf :: Maze -> Coord
generateLeaf maze = do
    let (k:ks)  = maze
        numRows = length maze
        numCols = length k
        x = unsafePerformIO $ randomRIO (1, numRows - 1)
        y = unsafePerformIO $ randomRIO (1, numCols - 1)
    if maze !! x !! y == Path
        then (x, y)
        else generateLeaf maze

generateLeaves :: Maze -> [Coord] -> Int -> [Coord]
generateLeaves maze leaves ll
    | ll == 0 = leaves
    | otherwise =
        let newLeaf = generateLeaf maze
        in if newLeaf `elem` leaves
            then generateLeaves maze leaves ll
            else generateLeaves maze (newLeaf:leaves) (ll-1)

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
