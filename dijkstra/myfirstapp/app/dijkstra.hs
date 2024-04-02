module Dijkstra (dijkstra) where

permutations ::  [Int] -> [[Int]]
permutations [] = [[]]
permutations xs = [ x:ys | x <- xs, ys <- permutations (remove x xs)]
  where
    remove :: Int -> [Int] -> [Int]
    remove _ [] = []
    remove y (x:xs)
      | y == x   = remove y xs
      | otherwise = x : remove y xs

dijkstra :: [[Int]] -> [Int] -> Int -> IO Int
dijkstra matriz frutas lengthFrutas = do
  let frutasPermutations = permutations frutas
  putStrLn "Frutas Permutations:"
  print frutasPermutations
  return 0













    
