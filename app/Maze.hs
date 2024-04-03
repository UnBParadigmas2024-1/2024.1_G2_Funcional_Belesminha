module Maze(Maze) where
import System.Random ( randomRIO )
import Map(mazeMap,Cell(..))

type Maze = [[Cell]]
type Coord = (Int,Int)

viewMaze :: Maze -> IO ()
viewMaze maze = mapM_ putStrLn $ map (concatMap showCell) maze
    where
        showCell Wall   = "■ "
        showCell Path   = "⬚ "
        showCell Start  = "S "
        showCell End    = "E "

emptyMaze :: Coord -> Maze
emptyMaze dimensions = replicate rows $ replicate cols Wall
    where
        (rows,cols) = dimensions

updateMaze :: Maze -> Coord -> Cell -> Maze
updateMaze maze (x,y) cll = (a ++ [l ++ cll:r] ++ b)
    where
        (a,row:b)  = splitAt x maze
        (l,col:r)  = splitAt y row

getNeighbors :: Maze -> Coord -> [Coord]
getNeighbors maze (x,y) = filter (\coords -> inBounds maze coords) neighborCoords
    where
        neighborCoords :: [Coord]
        neighborCoords = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

        inBounds :: Maze -> Coord -> Bool
        inBounds maze (x,y) = x >= 0 && y >= 0 && x < length maze && y < length cols
            where
                (cols:rows) = maze

randomElem :: [a] -> IO a
randomElem xs = do
        i <- randomRIO (0, length xs - 1)
        return $ xs !! i