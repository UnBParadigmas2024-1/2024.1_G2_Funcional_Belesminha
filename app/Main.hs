import Graphics.Gloss
import World ( sampleWorld, mazeToPicture, handleInput, initializeWorld )
import Maze ( generateLeaves, updateMaze )
import Map ( mazeMap, Cell(Leaf) )

main :: IO ()
main = do
    newWorld <- initializeWorld
    play
        (InWindow "Belesminha" (600,600) (0,0))
        white
        60
        newWorld
        mazeToPicture
        handleInput
        (\time nothing -> nothing)