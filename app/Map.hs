module Map(Cell(..),mazeMap) where
data Cell = MinStep | Wall | Path | Start | Leaf | End deriving (Eq)

-- cellToPicture (MinStep n) = color (makeColor 0.5 0.5 0.5 1) $ rectangleSolid cellSize cellSize
instance Show Cell where
    show Wall    = "#"
    show Start   = "S"
    show End     = "E"
    show Path    = "o"
    show Leaf    = "L"
mazeMap =
    [
        [Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall]
        ,[Wall,Start,Wall,Path,Path,Wall,Wall,Wall,Wall,Path,Path,Path,Wall,Wall,Path,Path,Path,Path,Path,Path,Path,Path,Path,Wall]
        ,[Wall,Path,Wall,Path,Path,Path,Wall,Wall,Path,Path,Path,Path,Path,Path,Path,Path,Wall,Wall,Path,Path,Wall,Wall,Path,Wall]
        ,[Wall,Path,Path,Path,Path,Path,Path,Path,Path,Path,Wall,Path,Path,Wall,Path,Path,Wall,Wall,Path,Wall,Wall,Path,Path,Wall]
        ,[Wall,Path,Path,Path,Wall,Path,Path,Wall,Wall,Wall,Wall,Path,Path,Wall,Path,Path,Path,Path,Path,Path,Path,Path,Path,Wall]
        ,[Wall,Path,Path,Wall,Wall,Wall,Path,Path,Path,Path,Path,Path,Path,Wall,Wall,Path,Path,Path,Path,Wall,Wall,Wall,Path,Wall]
        ,[Wall,Path,Path,Path,Wall,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Wall,Path,Wall]
        ,[Wall,Path,Wall,Path,Path,Path,Path,Wall,Wall,Wall,Path,Wall,Wall,Path,Path,Path,Wall,Path,Path,Wall,Wall,Wall,Path,Wall]
        ,[Wall,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Wall,Wall,Path,Path,Path,Wall,Wall,Wall,Wall,Wall,Wall,Path,Wall]
        ,[Wall,Path,Wall,Wall,Wall,Wall,Wall,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Wall]
        ,[Wall,Path,Path,Path,Path,Path,Wall,Path,Path,Path,Path,Path,Path,Path,Wall,Path,Path,Path,Wall,Path,Path,Wall,Path,Wall]
        ,[Wall,Path,Path,Wall,Wall,Path,Wall,Path,Path,Wall,Wall,Wall,Path,Path,Wall,Wall,Path,Path,Wall,Path,Wall,Wall,Path,Wall]
        ,[Wall,Path,Path,Wall,Wall,Path,Wall,Path,Path,Wall,Path,Path,Path,Path,Path,Path,Path,Path,Wall,Path,Path,Path,Path,Wall]
        ,[Wall,Path,Path,Path,Path,Path,Path,Path,Path,Wall,Path,Path,Path,Wall,Wall,Path,Path,Wall,Wall,Wall,Path,Path,Path,Wall]
        ,[Wall,Path,Wall,Wall,Wall,Path,Path,Wall,Wall,Wall,Path,Wall,Wall,Wall,Wall,Path,Path,Path,Path,Path,Path,Path,Path,Wall]
        ,[Wall,Path,Path,Path,Wall,Path,Path,Path,Path,Path,Path,Path,Wall,Wall,Path,Path,Path,Path,Path,Path,Wall,Wall,Path,Wall]
        ,[Wall,Path,Path,Path,Wall,Wall,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Wall,Wall,Path,Path,Wall,Path,Path,Wall]
        ,[Wall,Path,Path,Path,Path,Path,Path,Path,Path,Wall,Path,Path,Wall,Wall,Wall,Path,Wall,Wall,Path,Path,Path,Path,Path,Wall]
        ,[Wall,Path,Wall,Path,Path,Path,Wall,Path,Wall,Wall,Wall,Path,Path,Wall,Path,Path,Path,Path,Path,Wall,Path,Path,Path,Wall]
        ,[Wall,Path,Wall,Path,Path,Wall,Wall,Path,Path,Wall,Path,Path,Path,Wall,Wall,Wall,Path,Path,Wall,Wall,Wall,Wall,Path,Wall]
        ,[Wall,Path,Wall,Path,Wall,Wall,Wall,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Wall]
        ,[Wall,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Wall,Wall,Wall,Path,Path,Wall,Path,Wall]
        ,[Wall,Path,Wall,Wall,Path,Wall,Wall,Path,Path,Wall,Wall,Wall,Path,Wall,Wall,Path,Wall,Wall,Wall,Wall,Path,Wall,Path,Wall]
        ,[Wall,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Wall,End,Path,Path,Path,Path,Path,Wall]
        ,[Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall]
    ]