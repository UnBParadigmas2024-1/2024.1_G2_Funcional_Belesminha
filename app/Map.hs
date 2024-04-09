module Map(Cell(..),mazeMap,cellSize,cellToPicture) where

import Graphics.Gloss
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

cellSize :: Float
cellSize = 20

cellToPicture :: Cell -> Picture
cellToPicture Wall = color black $ rectangleSolid cellSize cellSize
cellToPicture Path = color white $ rectangleSolid cellSize cellSize
cellToPicture Start = color green $ rectangleSolid cellSize cellSize
cellToPicture End = color red $ rectangleSolid cellSize cellSize
cellToPicture Leaf = color orange $ rectangleSolid cellSize cellSize
cellToPicture MinStep = color (makeColor 0.5 0.5 0.5 1) $ rectangleSolid cellSize cellSize