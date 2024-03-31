-- {-# LANGUAGE FlexibleContexts #-}

module Maze where 
import System.Random
import Control.Lens
import Control.Monad

data Cell = Cell { 
    north :: Bool,
    south :: Bool,
    east :: Bool,
    west :: Bool,
    visited :: Bool
 } deriving (Eq, Show)

type Maze = [[Cell]]

-- Generate an empty maze with dimensions rows x cols
emptyMaze :: Int -> Int -> Maze
emptyMaze rows cols = replicate rows (replicate cols emptyCell)
  where emptyCell = Cell True True True True False

-- Check if a cell is within the bounds of the maze
-- Goes from 0 to (n-1), i.e. [0,rows-1] and [0,cols-1]
inBounds :: Int -> Int -> Maze -> Bool
-- length maze        = rows
-- length (head maze) = cols
inBounds x y maze = x >= 0 && y >= 0 && x < length maze && y < length (head maze)

-- Get the neighbors of a cell
getNeighbors :: Int -> Int -> Maze -> [(Int, Int)]
-- uncurry inBounds makes it receive (Int,Int) -> Maze
-- \coords is a lambda which is passed to uncurried inBounds with maze
-- neighborCoords gets adjacent cartesian values to a point into a coordinate [rows-1, cols-1]
getNeighbors x y maze = filter (\coords -> (uncurry inBounds) coords maze) neighborCoords
    where
        neighborCoords = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

-- Remove element from a list using its index
removeAt :: Int -> [Int] -> ([Int], Int)
removeAt _ [] = error "removeAt: index too large"
removeAt 0 (x:xs) = (xs, x)
removeAt n (x:xs) = let (rest, y) = removeAt (n - 1) xs
                    in (x : rest, y)

-- Generate a random permutation of a list
shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
  randIndex <- randomRIO (0, length xs - 1)  -- Generate a random index
  let (left, (x:right)) = splitAt randIndex xs  -- Split the list at the random index
  shuffledRight <- shuffle (left ++ right)  -- Recursively shuffle the right part
  return (x:shuffledRight)  -- Return the shuffled list

getCell :: Int -> Int -> Maze -> Cell
getCell x y maze = (maze !! x) !! y

getCellsFromNeighbors :: [(Int, Int)] -> [[Cell]] -> [Cell]
getCellsFromNeighbors neighbors maze = map (\(x, y) -> getCell x y maze) neighbors

setVisited :: Bool -> Cell -> Cell
setVisited v c = c {visited=v}

labirinto = [[Cell {north = False, south = False, east = False, west = False, visited = True},Cell {north = True, south = True, east = True, west = False, visited = True},Cell {north = True, south = True, east = True, west = True, visited = False},Cell {north = True, south = True, east = True, west = True, visited = False},Cell {north = True, south = True, east = True, west = True, visited = False},Cell {north = True, south = True, east = True, west = True, visited = False}],[Cell {north = True, south = True, east = True, west = True, visited = False},Cell {north = True, south = True, east = True, west = True, visited = False},Cell {north = True, south = True, east = True, west = True, visited = False},Cell {north = True, south = True, east = True, west = True, visited = False},Cell {north = True, south = True, east = True, west = True, visited = False},Cell {north = True, south = True, east = True, west = True, visited = False}]]
vizinhos = getNeighbors 0 0 labirinto
celulas = getCellsFromNeighbors vizinhos labirinto
filtrados = filter (not . visited) celulas
embaralhado = shuffle filtrados

pairs = shuffle $ filter (\(_,cell) -> not (visited cell)) (zip vizinhos celulas)

-- shuffle $ filter (not.visited) (getCellsFromNeighbors (getNeighbors 0 0 labirinto) labirinto)

-- updateMaze :: Int -> Int -> Maze -> Maze
-- updateMaze nx ny maze = do 
--     let maze'' = maze & ix nx . ix ny %~ setVisited True
--     generateMaze nx ny rows cols maze''

generateMaze :: Int -> Int -> Int -> Int -> Maze -> IO Maze
generateMaze x y rows cols maze = do
    let indexes = getNeighbors x y maze
    let cells = getCellsFromNeighbors indexes maze
    let pairs = zip indexes cells
    let neighbors = shuffle $ filter (\(_, cell) -> not (visited cell)) pairs
    maze' <- foldM (\maze (nx, ny) -> do
                        let maze'' = maze & ix nx . ix ny %~ setVisited True
                        generateMaze nx ny rows cols maze'') maze (fmap fst (fmap unzip neighbors))
    return maze'
--   where
--     getCell x y = (maze !! y) !! x
--     cellAt x y = ix y . ix x
--     setVisited v cell = cell { visited = v }
--     removeWall (x1, y1) (x2, y2) cell
--       | x2 > x1 = cell { east = False }
--       | x2 < x1 = cell { west = False }
--       | y2 > y1 = cell { south = False }
--       | y2 < y1 = cell { north = False }
--       | otherwise = cell

-- Recursive backtracking algorithm to generate the maze
-- generateMaze :: Int -> Int -> Int -> Int -> Maze -> IO Maze
-- generateMaze x y rows cols maze = do
--   let neighbors = shuffle $ filter (not . visited . getCell) (getNeighbors x y maze)
--   maze' <- foldM (\maze (nx, ny) -> do
--                     let maze'' = maze & cellAt x y . setVisited True & cellAt nx ny . removeWall (x, y) (nx, ny)
--                     generateMaze nx ny rows cols maze'') maze neighbors
--   return maze'
--   where
--     getCell x y = (maze !! y) !! x
--     cellAt x y = ix y . ix x
--     setVisited v cell = cell { visited = v }
--     removeWall (x1, y1) (x2, y2) cell
--       | x2 > x1 = cell { east = False }
--       | x2 < x1 = cell { west = False }
--       | y2 > y1 = cell { south = False }
--       | y2 < y1 = cell { north = False }
--       | otherwise = cell
