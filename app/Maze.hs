{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use elemIndex" #-}
{-# LANGUAGE BlockArguments #-}
module Maze(Maze, updateMaze, generateLeaves, startCoord, incrementS, incrementA, incrementW, incrementD ) where
import System.Random ( randomRIO )
import Map(mazeMap,Cell(..))
import Control.Monad (replicateM)
import Data.List (findIndex)
import Data.Maybe (fromMaybe)
import Control.Monad.State (execState, State, MonadState (get, put))

type Maze = [[Cell]]
type Coord = (Int,Int)

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
