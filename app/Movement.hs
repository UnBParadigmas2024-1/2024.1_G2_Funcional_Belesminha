module Movement(
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