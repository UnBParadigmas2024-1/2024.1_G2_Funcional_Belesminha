import Graphics.Gloss
import World (mazeToPicture, handleInput, initializeWorld, updateWorld, moveCount)
import Dijkstra (calculateMinSteps)
import Maze (generateLeaves)
import Map (mazeMap)
import Ranking (showRanking, writeRanking)


main :: IO ()
main = do
    -- Mostrar o ranking antes de iniciar o jogo
    showRanking "ranking.txt"
    _ <- getLine


    let initialMaze = mazeMap

    leaves <- generateLeaves initialMaze
    newWorld <- initializeWorld leaves
    minSteps <- calculateMinSteps newWorld leaves initialMaze

    play
        (InWindow "Belesminha" (600, 600) (0, 0))
        black
        60
        newWorld
        (mazeToPicture minSteps)
        handleInput
        updateWorld
    
    -- Atualizar o ranking
    writeRanking (moveCount newWorld)