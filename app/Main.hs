import Graphics.Gloss
import System.IO
import World (mazeToPicture, handleInput, initializeWorld, updateWorld)
import Dijkstra (calculateMinSteps)
import Maze (generateLeaves)
import Map (mazeMap)

main :: IO ()
main = do
    -- Abrir o arquivo e ler o conteúdo
    handle <- openFile "ranking.txt" ReadMode
    contents <- hGetContents handle
    let ranking = lines contents -- Separa as linhas do arquivo em uma lista de strings

    -- Mostrar o ranking na tela
    putStrLn "Ranking:"
    mapM_ putStrLn ranking
    putStrLn "Pressione Enter para iniciar o jogo."

    -- Fechar o arquivo após a leitura
    hClose handle

    -- Aguardar a entrada do jogador antes de iniciar o jogo
    _ <- getLine

    -- Resto do seu código
    let initialMaze = mazeMap

    leaves <- generateLeaves initialMaze
    newWorld <- initializeWorld leaves
    minSteps <- calculateMinSteps newWorld leaves initialMaze

    -- Iniciar o jogo
    play
        (InWindow "Belesminha" (600, 600) (0, 0))
        black
        60
        newWorld
        (mazeToPicture minSteps)
        handleInput
        updateWorld
