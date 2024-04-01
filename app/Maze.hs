module Maze() where
import System.Random

type Maze = [[Cell]]
type Coord = (Int,Int)
data Cell = Wall | Path | Start | End deriving (Eq)

instance Show Cell where
    show Wall    = "#"
    show Start   = " "
    show End     = "E"
    show Path    = "S"

viewMaze :: Maze -> IO ()
viewMaze maze = mapM_ putStrLn $ map (concatMap showCell) maze
    where
        showCell Wall   = "■ "
        showCell Path   = "⬚ "
        showCell Start  = "S "
        showCell End    = "E "

maze =
    [
      [Wall, Wall , Wall, Wall, Wall]
    , [Wall, Start, Path, Path, Wall]
    , [Wall, Wall , Path, Wall, Wall]
    , [Wall, Path , Path, Path, Wall]
    , [Wall, Wall , Wall, End , Wall]
    ]

emptyMaze :: Coord -> Maze
emptyMaze dimensions = replicate rows $ replicate cols Wall
    where
        (rows,cols) = dimensions

updateMaze :: Maze -> Coord -> Cell -> Maze
updateMaze maze (x,y) cll = (a ++ [l ++ cll:r] ++ b)
    where
        (a,row:b)  = splitAt x maze
        (l,col:r)  = splitAt y row