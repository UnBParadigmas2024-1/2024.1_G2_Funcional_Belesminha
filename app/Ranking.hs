module Ranking where

import System.IO

-- Função para ler o conteúdo do arquivo e mostrar o ranking na tela
showRanking :: FilePath -> IO ()
showRanking filePath = do
  -- Abrir o arquivo e ler o conteúdo
  handle <- openFile filePath ReadMode
  contents <- hGetContents handle
  let ranking = lines contents -- Separa as linhas do arquivo em uma lista de strings

  -- Mostrar o ranking na tela
  putStrLn "Ranking:"
  mapM_ putStrLn ranking
  putStrLn "Pressione Enter para iniciar o jogo."

  -- Fechar o arquivo após a leitura
  hClose handle
