import Graphics.Gloss
import Map ( mazeMap, Cell(..) )
import Maze ( Maze )

cellSize :: Float
cellSize = 20

cellToPicture :: Cell -> Picture
cellToPicture Wall  = color black $ rectangleSolid cellSize cellSize
cellToPicture Path  = color white $ rectangleSolid cellSize cellSize
cellToPicture Start = color green $ rectangleSolid cellSize cellSize
cellToPicture End   = color red   $ rectangleSolid cellSize cellSize

sampleWorld :: World
sampleWorld = World {
    worldMap = mazeMap
}

data World = World {
    worldMap :: Maze
}

mazeToPicture :: World -> Picture
mazeToPicture world = 
    let maze = worldMap world
        reversedMaze = reverse maze
    in translate (-240) (-225) . pictures $
    [ translate (x * cellSize) (y * cellSize) (cellToPicture cell)
    | (y,  row) <- zip [0..] reversedMaze
    , (x, cell) <- zip [0..] row
    ]

main :: IO ()
main = play
    (InWindow "Belesminha" (600,600) (0,0))
    white
    30
    sampleWorld
    mazeToPicture
    (\_ nothing -> nothing)
    (\_ nothing -> nothing)