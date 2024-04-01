module Maze(
    Cell,
    Maze,
    Coord,
) where

data Cell = Cell { 
    up :: Bool,
    down :: Bool,
    right :: Bool,
    left :: Bool,
    visited :: Bool
 } deriving (Eq, Show)

type Maze = [[Cell]]
type Coord = (Int,Int)

emptyMaze :: Int -> Int -> Maze
emptyMaze rows cols = (replicate rows (replicate cols emptyCell))
    where emptyCell = Cell {
        up = False,
        down = False,
        right = False,
        left = False,
        visited = False
    }

inBounds :: Maze -> Coord -> Bool
inBounds maze (x,y) = 
    x >= 0 && y >= 0 && x < length (row:rows) && y < length (row)
    where (row:rows) = maze