module Maze() where
import System.Random
import Control.Monad (foldM)
    
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

maze = emptyMaze 4 4

generateRandomNum :: (Int,Int) -> IO Int
generateRandomNum (min,max) = randomRIO (min,max)

getCarveDirection :: Directions -> IO (Changer Cell)
getCarveDirection pos = do
    case pos of
        ToUp    -> return (\new_dir -> new_dir { up = True    , visited = True })
        ToDown  -> return (\new_dir -> new_dir { down = True  , visited = True })
        ToLeft  -> return (\new_dir -> new_dir { left = True  , visited = True })
        ToRight -> return (\new_dir -> new_dir { right = True , visited = True })
        None    -> return (\new_dir -> new_dir { visited = True })

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
    let (a,row:b)  = splitAt x maze
        (l,cll:r)  = splitAt y row
    in (a ++ [l ++ (ch cll):r] ++ b)

getNeighborsDirections :: Maze -> Coord -> [Directions]
getNeighborsDirections maze coord = map (findDirection coord) neighbors
    where
        neighbors = getNeighbors maze coord

getNeighbors :: Maze -> Coord -> [Coord]
getNeighbors maze (x,y) = filter (\coords -> (inBounds) maze coords) neighborCoords
    where
        neighborCoords :: [Coord]
        neighborCoords = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

inBounds :: Maze -> Coord -> Bool
inBounds maze (x,y) = x >= 0 && y >= 0 && x < length maze && y < length (head maze)

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


elements :: [a] -> IO a
elements xs = do
    i <- randomRIO (0, length xs - 1)
    return $ xs !! i

createMaze :: Maze -> IO Maze
createMaze maze = dfs maze (0,0) []

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

ms = [[Cell {up = False, down = False, right = False, left = False, visited = False},Cell {up = False, down = False, right = False, left = False, visited = True},Cell {up = False, down = False, right = False, left = False, visited = False},Cell {up = False, down = False, right = False, left = False, visited = False}],[Cell {up = False, down = False, right = False, left = False, visited = False},Cell {up = False, down = False, right = False, left = False, visited = False},Cell {up = False, down = False, right = False, left = False, visited = False},Cell {up = False, down = False, right = False, left = False, visited = False}],[Cell {up = False, down = False, right = False, left = False, visited = False},Cell {up = False, down = False, right = False, left = False, visited = False},Cell {up = False, down = False, right = False, left = False, visited = False},Cell {up = False, down = False, right = False, left = False, visited = False}],[Cell {up = False, down = False, right = False, left = False, visited = False},Cell {up = False, down = False, right = False, left = False, visited = False},Cell {up = False, down = False, right = False, left = False, visited = False},Cell {up = False, down = False, right = False, left = False, visited = False}]]

dfs :: Maze -> Coord -> [Coord] -> IO Maze
dfs maze (x,y) stack = do
    let neighbors = filter (\(nx,ny) -> not (visited ((maze !! nx) !! ny))) (getNeighbors maze (x,y))
    let dirs = [findDirection (x,y) (xi,yi) | (xi,yi) <- neighbors]
    let nstack = if null neighbors then stack else ((x,y):stack)

    let max = if null dirs then 0 else length dirs - 1
    rn <- generateRandomNum (0,max)

    let pos = if null dirs then None else dirs !! rn
    dir <- getCarveDirection pos

    let maze' = updateMaze maze (x,y) dir
    if null nstack
        then return maze'
        else 
            case neighbors of
                [] -> do
                    let (nx,ny) = head nstack
                    dfs maze' (nx,ny) (tail nstack)
                _  -> do
                    let (nx,ny) = goToNeighbor maze' (x,y) pos
                    opPos <- getCarveDirection (getComplementaryDir maze' (x,y) pos)
                    dfs (updateMaze maze' (x,y) opPos) (nx,ny) ((x,y):nstack)

printMaze :: Maze -> IO ()
printMaze maze = do
    let cols = length (maze !! 0)
    putStrLn $ replicate ((cols * 4) + 1) '+'
    mapM_ printRow maze
    where
        printRow :: [Cell] -> IO ()
        printRow cells = do
            let cols = length (maze !! 0)
            putStr "|"
            mapM_ printCell cells
            putStrLn ""
            putStrLn $ replicate ((cols * 4) + 1) '+'
        
        printCell :: Cell -> IO ()
        printCell cell = do
            putStr $ if right cell && down cell then "_|" else "|"
            putStr " "
            putStr $ if down cell then "  " else "--"
            putStr " "
