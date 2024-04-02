module Map(Cell(..),mazeMap) where
data Cell = Wall | Path | Start | End deriving (Eq)
instance Show Cell where
    show Wall    = "#"
    show Start   = "S"
    show End     = "E"
    show Path    = "o"
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