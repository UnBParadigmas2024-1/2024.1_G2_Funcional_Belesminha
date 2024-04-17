import Dijkstra (calculateMinSteps)
import Graphics.Gloss
import Map (mazeMap)
import Maze (generateLeaves)
import Ranking (showRanking, writeRanking)
import World (World (..), handleInput, initializeWorld, mazeToPicture, updateWorld)

main :: IO ()
main = do
  -- Mostrar o ranking antes de iniciar o jogo
  showRanking "ranking.txt"
  _ <- getLine

  let initialMaze = mazeMap

  leaves <- generateLeaves initialMaze
  newWorld' <- initializeWorld leaves
  minSteps <- calculateMinSteps newWorld' leaves initialMaze

  let newWorld = newWorld' {maxSteps = length minSteps}
  play
    (InWindow "Belesminha" (1000, 1000) (0, 0))
    black
    60
    newWorld
    (mazeToPicture minSteps)
    handleInput
    updateWorld
    

  writeRanking (moveCount newWorld)