module Dijkstra(permutation,calculateFullPath) where

import Data.List (foldl')
import qualified Data.Set as Set

import Map (Cell(..))
import Maze (Coord,Maze)

permutation :: [Coord] -> [[Coord]]
permutation [] = [[]]
permutation xs = [ x:ys | x <- xs, ys <- permutation (delete x xs)]
      where
        delete :: Coord -> [Coord] -> [Coord]
        delete _ [] = []
        delete y (x:xs)
          | y == x   = delete y xs
          | otherwise = x : delete y xs

getValidNeighbors :: Coord -> Maze -> [Coord]
getValidNeighbors (x, y) maze = filter (\(x', y') -> x' >= 0 && y' >= 0 && x' < length maze && y' < length k && maze !! x' !! y' /= Wall) [(x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)]
    where
        (k:ks) = maze

dijkstra :: Coord -> Coord -> Maze -> Maybe [Coord]
dijkstra start end maze = dijkstra' (Set.singleton (0, start, [start])) Set.empty
    where
        dijkstra' :: Set.Set (Int, Coord, [Coord]) -> Set.Set Coord -> Maybe [Coord]
        dijkstra' pq visited
            | Set.null pq = Nothing
            | otherwise = let ((cost, pos, path), pq') = Set.deleteFindMin pq in
                if pos == end then Just path
                else if Set.member pos visited then dijkstra' pq' visited
                else let neighbors = getValidNeighbors pos maze in
                    dijkstra' (foldl' (\acc neighbor -> Set.insert (cost + 1, neighbor, path ++ [neighbor]) acc) pq' neighbors) (Set.insert pos visited)

findPathToDestination :: Coord -> Coord -> Maze -> Maybe [Coord]
findPathToDestination start end maze = dijkstra start end maze

calculateFullPath :: Coord -> [Coord] -> Coord -> Maze -> Maybe [Coord]
calculateFullPath start [] end maze = findPathToDestination start end maze
calculateFullPath start (fruta:outrasFrutas) end maze = do
    pathToFruta <- findPathToDestination start fruta maze
    pathToRest <- calculateFullPath fruta outrasFrutas end maze
    let x:xs = pathToRest
    return (pathToFruta ++ xs)
