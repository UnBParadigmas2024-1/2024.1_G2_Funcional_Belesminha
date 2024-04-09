module Dijkstra(calculateMinSteps, endPos, startPos) where
import qualified Data.Set as Set
import Data.Maybe (fromJust)
import Data.List (minimumBy, foldl')
import Data.Function (on)
import Map ( mazeMap, Cell(Leaf, Wall) )
import Maze (Coord)
import World (World(..))

type Position = (Int, Int)

permutation :: [Position] -> [[Position]]
permutation [] = [[]]
permutation xs = [ x:ys | x <- xs, ys <- permutation (delete x xs)]
      where
        delete :: Position -> [Position] -> [Position]
        delete _ [] = []
        delete y (x:xs)
          | y == x   = delete y xs
          | otherwise = x : delete y xs

-- Retorna a lista de vizinhos válidos de uma posição
getValidNeighbors :: Position -> [[Cell]] -> [Position]
getValidNeighbors (x, y) maze = filter (\(x', y') -> x' >= 0 && y' >= 0 && x' < length maze && y' < length (k) && maze !! x' !! y' /= Wall) [(x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)]
    where
        (k:ks) = maze

-- Algoritmo de Dijkstra
dijkstra :: Position -> Position -> [[Cell]] -> Maybe [Position]
dijkstra start end maze = dijkstra' (Set.singleton (0, start, [start])) Set.empty
    where
        dijkstra' :: Set.Set (Int, Position, [Position]) -> Set.Set Position -> Maybe [Position]
        dijkstra' pq visited
            | Set.null pq = Nothing
            | otherwise = let ((cost, pos, path), pq') = Set.deleteFindMin pq in
                if pos == end then Just path
                else if Set.member pos visited then dijkstra' pq' visited
                else let neighbors = getValidNeighbors pos maze in
                    dijkstra' (foldl' (\acc neighbor -> Set.insert (cost + 1, neighbor, path ++ [neighbor]) acc) pq' neighbors) (Set.insert pos visited)

-- Função para encontrar o caminho de start até um destino específico
findPathToDestination :: Position -> Position -> [[Cell]] -> Maybe [Position]
findPathToDestination start end maze = dijkstra start end maze

-- Função para calcular o caminho total passando por todas as frutas
calculateFullPath :: Position -> [Position] -> Position -> [[Cell]] -> Maybe [Position]
-- Se não houver frutas, calcula o caminho direto de start até end
calculateFullPath start [] end maze = findPathToDestination start end maze
calculateFullPath start (fruta:outrasFrutas) end maze = do
    pathToFruta <- findPathToDestination start fruta maze
    pathToRest <- calculateFullPath fruta outrasFrutas end maze
    let x:xs = pathToRest
    return (pathToFruta ++ xs)

calculateMinSteps :: World -> [Position] -> [[Cell]] -> IO [Position]
calculateMinSteps world leaves maze =
    let fpermutations = permutation leaves
        startPos' = startPos world
        endPos' = endPos world
        allPaths = map (\fruitPermut -> calculateFullPath startPos' fruitPermut endPos' maze) fpermutations
        allPaths' = map fromJust allPaths
        minPath = minimumBy (compare `on` length) allPaths'
        in return minPath