module Main where
import qualified Data.Set as Set
import Data.Maybe (fromJust)
import Data.List (minimumBy, foldl')
import Data.Function (on)

type Position = (Int, Int)
data Cell = Wall | Path | Start | End deriving (Eq, Show)

mazeMap :: [[Cell]]
mazeMap =
    [
        [Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall]
        ,[Wall,Start,Wall,Path,Path,Wall,Wall,Wall,Wall,Path,Path,Path,Wall,Wall,Path,Path,Path,Path,Path,Path,Path,Path,Path,Wall]
        ,[Wall,Path,Wall,Path,Path,Path,Wall,Wall,Path,Path,Path,Path,Path,Path,Path,Path,Wall,Wall,Path,Path,Wall,Wall,Path,Wall]
        ,[Wall,Path,Path,Path,Path,Path,Path,Path,Path,Path,Wall,Path,Path,Wall,Path,Path,Wall,Wall,Path,Wall,Wall,Path,Path,Wall]
        ,[Wall,Path,Path,Path,Wall,Path,Path,Wall,Wall,Wall,Wall,Path,Path,Wall,Path,Path,Path,Path,Path,Path,Path,Path,Path,Wall]
        ,[Wall,Path,Path,Wall,Wall,Wall,Path,Path,Path,Path,Path,Path,Path,Wall,Wall,Path,Path,Path,Path,Wall,Wall,Wall,Path,Wall]
        ,[Wall,Path,Path,Path,Wall,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Wall,Path,Wall]
        ,[Wall,Path,Wall,Path,Path,Path,Path,Wall,Wall,Wall,Path,Wall,Wall,Path,Path,Path,Wall,Path,Path,Wall,Wall,Wall,Path,Wall]
        ,[Wall,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Wall,Wall,Path,Path,Path,Wall,Wall,Wall,Wall,Wall,Wall,Path,Wall]
        ,[Wall,Path,Wall,Wall,Wall,Wall,Wall,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Wall]
        ,[Wall,Path,Path,Path,Path,Path,Wall,Path,Path,Path,Path,Path,Path,Path,Wall,Path,Path,Path,Wall,Path,Path,Wall,Path,Wall]
        ,[Wall,Path,Path,Wall,Wall,Path,Wall,Path,Path,Wall,Wall,Wall,Path,Path,Wall,Wall,Path,Path,Wall,Path,Wall,Wall,Path,Wall]
        ,[Wall,Path,Path,Wall,Wall,Path,Wall,Path,Path,Wall,Path,Path,Path,Path,Path,Path,Path,Path,Wall,Path,Path,Path,Path,Wall]
        ,[Wall,Path,Path,Path,Path,Path,Path,Path,Path,Wall,Path,Path,Path,Wall,Wall,Path,Path,Wall,Wall,Wall,Path,Path,Path,Wall]
        ,[Wall,Path,Wall,Wall,Wall,Path,Path,Wall,Wall,Wall,Path,Wall,Wall,Wall,Wall,Path,Path,Path,Path,Path,Path,Path,Path,Wall]
        ,[Wall,Path,Path,Path,Wall,Path,Path,Path,Path,Path,Path,Path,Wall,Wall,Path,Path,Path,Path,Path,Path,Wall,Wall,Path,Wall]
        ,[Wall,Path,Path,Path,Wall,Wall,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Wall,Wall,Path,Path,Wall,Path,Path,Wall]
        ,[Wall,Path,Path,Path,Path,Path,Path,Path,Path,Wall,Path,Path,Wall,Wall,Wall,Path,Wall,Wall,Path,Path,Path,Path,Path,Wall]
        ,[Wall,Path,Wall,Path,Path,Path,Wall,Path,Wall,Wall,Wall,Path,Path,Wall,Path,Path,Path,Path,Path,Wall,Path,Path,Path,Wall]
        ,[Wall,Path,Wall,Path,Path,Wall,Wall,Path,Path,Wall,Path,Path,Path,Wall,Wall,Wall,Path,Path,Wall,Wall,Wall,Wall,Path,Wall]
        ,[Wall,Path,Wall,Path,Wall,Wall,Wall,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Wall]
        ,[Wall,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Path,Wall,Path,Path,Path,Path,Path,End,Wall]
        ,[Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall]
    ]
    


-- Retorna a lista de vizinhos válidos de uma posição
getValidNeighbors :: Position -> [[Cell]] -> [Position]
getValidNeighbors (x, y) maze = filter (\(x', y') -> x' >= 0 && y' >= 0 && x' < length maze && y' < length (head maze) && maze !! x' !! y' /= Wall) [(x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)]


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

main :: IO ()
main = do
  let frutas = [(5, 2), (13, 7), (20, 9)] -- Ou qualquer outra lista de frutas que você deseja testar
  let start = (1, 1)
  let end = (21, 22)
  let path = dijkstra start (5, 2) mazeMap
  case path of
    Just p -> putStrLn $ "Caminho: " ++ show p
    Nothing -> putStrLn "Não foi possível encontrar um caminho"
