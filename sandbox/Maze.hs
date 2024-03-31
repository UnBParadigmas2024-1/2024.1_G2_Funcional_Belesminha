module Maze where
import System.Random

data Cell = Cell { 
    up :: Bool,
    down :: Bool,
    right :: Bool,
    left :: Bool,
    visited :: Bool
 } deriving (Eq, Show)

type Maze = [[Cell]]
type Coord = (Int,Int)
type Changer a = (a -> a)

lab = [[Cell {up = False, down = False, right = False, left = False, visited = True},Cell {up = True, down = True, right = True, left = True, visited = True},Cell {up = True, down = True, right = True, left = True, visited = True},Cell {up = False, down = False, right = False, left = False, visited = False},Cell {up = False, down = False, right = False, left = False, visited = False},Cell {up = False, down = False, right = False, left = False, visited = False}],[Cell {up = False, down = False, right = False, left = False, visited = False},Cell {up = False, down = False, right = False, left = False, visited = False},Cell {up = False, down = False, right = False, left = False, visited = False},Cell {up = False, down = False, right = False, left = False, visited = False},Cell {up = False, down = False, right = False, left = False, visited = False},Cell {up = False, down = False, right = False, left = False, visited = False}]]
new = Cell {
        up = False,
        down = False,
        right = False,
        left = False,
        visited = False
    }

generateRandomZeroOrOne :: IO Int
generateRandomZeroOrOne = randomRIO (0, 1)

getCarveDirection :: IO(Changer Cell)
getCarveDirection = do
    rn <- generateRandomZeroOrOne
    if rn == 0 then return (\new_dir -> new_dir { up = True, visited = True })
    else return (\new_dir -> new_dir { left = True, visited = True })

emptyMaze :: Int -> Int -> Maze
emptyMaze rows cols = (replicate rows (replicate cols emptyCell))
    where emptyCell = Cell {
        up = False,
        down = False,
        right = False,
        left = False,
        visited = False
    }

updateMaze :: Maze -> Coord -> Changer Cell -> Maze
updateMaze maze (x,y) ch =
    let (a,row:b)  = splitAt y maze
        (l,cll:r) = splitAt x row
    in (a ++ [l ++ (ch cll):r] ++ b)

createMaze :: Maze -> IO Maze
createMaze maze = do
  let numRows = length maze
      numCols = length (head maze)
  newMaze <- updateMaze maze (0,0) <$> getCarveDirection
  return newMaze


    
