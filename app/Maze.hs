{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use elemIndex" #-}
{-# LANGUAGE BlockArguments #-}
module Maze(Maze, updateMaze, generateLeaves, startCoord, incrementS, incrementA, incrementW, incrementD, Coord, Directions(..), goToNeighbor) where
import System.Random ( randomRIO )
import Map(mazeMap,Cell(..))
import Control.Monad (replicateM)
import Data.List (findIndex)
import Data.Maybe (fromMaybe)

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

findStart :: Maze -> Maybe Coord
findStart maze = findStart' maze 0
 where
    findStart' :: Maze -> Int -> Maybe Coord
    findStart' [] _ = Nothing
    findStart' (row:rows) rowIndex =
        case findIndex (==Start) row of
            Just colIndex -> Just (colIndex, rowIndex)
            Nothing -> findStart' rows (rowIndex + 1)

-- Start Coord
startCoord :: Coord
startCoord = fromMaybe (0, 0) (findStart mazeMap)

-- Increment S
incrementS :: Coord -> Coord
incrementS (y, x) = (y + 2, x)

-- Increment W 
incrementW :: Coord -> Coord
incrementW (y, x) = (y - 2, x)

-- Increment A
incrementA :: Coord -> Coord
incrementA (y, x) = (y, x - 2)

-- Increment D
incrementD :: Coord -> Coord
incrementD (y, x) = (y, x + 2)

emptyMaze :: Coord -> Maze
emptyMaze dimensions = replicate rows $ replicate cols Wall
    where
        (rows,cols) = dimensions

updateMaze :: Maze -> Coord -> Cell -> Maze
updateMaze maze (x,y) cll = a ++ [l ++ cll:r] ++ b
    where
        (a,row:b)  = splitAt x maze
        (l,col:r)  = splitAt y row

randomElem :: [a] -> IO a
randomElem xs = do
        i <- randomRIO (0, length xs - 1)
        return $ xs !! i

numberOfLeaves :: Int
numberOfLeaves = 10

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