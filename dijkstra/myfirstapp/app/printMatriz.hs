module PrintMatriz (printMatriz) where
import Data.List(permutations)


printMatriz :: [[Int]] -> [Int] -> Int -> IO Int
printMatriz matriz frutas lengthFrutas = do
  let frutasPermutations = permutations frutas
  putStrLn "Frutas Permutations:"
  print frutasPermutations
  return 0
    
