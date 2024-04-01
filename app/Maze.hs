module Maze() where

data Cell = Cell { 
    up :: Bool,
    down :: Bool,
    right :: Bool,
    left :: Bool,
    visited :: Bool
 } deriving (Eq, Show)

type Coord = (Int,Int)
type Maze = [[Cell]]

data Directions =
    ToUp
    | ToDown
    | ToLeft
    | ToRight
    | None
    deriving (Eq)

instance Show Directions where
    show ToUp    = "Up"
    show ToDown  = "Down"
    show ToRight = "Right"
    show ToLeft  = "Left"
    show None    = "None"

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

getNeighborsDirections :: Maze -> Coord -> [Directions]
getNeighborsDirections maze coord = map (findDirection coord) neighbors
    where
        neighbors = getNeighbors maze coord

getNeighbors :: Maze -> Coord -> [Coord]
getNeighbors maze (x,y) = filter (\coords -> (inBounds) maze coords) neighborCoords
    where
        neighborCoords :: [Coord]
        neighborCoords = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

findDirection :: Coord -> Coord -> Directions
findDirection start end
    | (x1,y1) == (x0-1,y0) = ToUp
    | (x1,y1) == (x0+1,y0) = ToDown
    | (x1,y1) == (x0,y0+1) = ToRight
    | (x1,y1) == (x0,y0-1) = ToLeft
    | otherwise            = None
    where
        (x0,y0) = start
        (x1,y1) = end

goToNeighbor :: Maze -> Coord -> Directions -> Coord
goToNeighbor maze (x,y) dir =
    let (dx,dy) = directionOffset dir
        rows = (length maze) - 1
        cols = (length (maze !! 0)) - 1
        new_x = if x+dx < 0 then 0 else if x+dx > rows then rows else x+dx
        new_y = if y+dy < 0 then 0 else if y+dy > cols then cols else y+dy
    in (new_x,new_y)

directionOffset :: Directions -> Coord
directionOffset dir =
    case dir of
        ToUp    -> (-1,0)
        ToDown  -> (1, 0)
        ToRight -> (0, 1)
        ToLeft  -> (0,-1)
        None    -> (0, 0)

getOppositeDir :: Directions -> Directions
getOppositeDir dir
    | dir == ToUp    = ToDown
    | dir == ToDown  = ToUp
    | dir == ToRight = ToLeft
    | dir == ToLeft  = ToRight
    | dir == None    = None

getComplementaryDir :: Maze -> Coord -> Directions -> Directions
getComplementaryDir maze (x, y) dir = 
    let opDir = getOppositeDir dir
    in if goToNeighbor maze (x,y) opDir == (x, y)
        then None
        else opDir