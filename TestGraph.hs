module TestGraph (bestPaths) where

import Data.Function ((&))
import Data.Set (Set)
import qualified Data.Set as Set
import Graph
  ( FullEdge,
    Graph (vertices),
    Path,
    Vertex,
    fullEdgeData,
    fullEdges,
    toFullEdges,
  )
import GraphMatrices (allPathsMatrix)
import Matrix (Matrix (rows))

availablePaths :: (Ord e, Ord v) => Set (Vertex v) -> Graph e v -> Set (Path e v)
availablePaths vs graph =
  zip [0, 1 ..] (graph & vertices)
    & filter (\(_, v) -> Set.member v vs)
    & concatMap (\(n, _) -> rows (allPathsMatrix graph) !! n)
    & concat
    & filter (not . null . toFullEdges)
    & Set.fromList

bestVariants :: (Ord metric, Eq metric, Ord elem) => (elem -> metric) -> Set elem -> Set elem
bestVariants f elements =
  metrics
    & Set.filter (\(_, m) -> m == bestMetric)
    & Set.map fst
  where
    metrics = Set.map (\a -> (a, f a)) elements
    bestMetric = minimum $ Set.map snd metrics

allFullEdges :: (Ord v, Ord e) => Set (Path e v) -> Set (FullEdge e v)
allFullEdges paths = Set.fromList $ concatMap toFullEdges paths

bestPaths :: (Semigroup e, Ord e, Ord v, Eq e, Eq v) => [Vertex v] -> Graph e v -> [[Path e v]]
bestPaths vs graph =
  availablePaths (Set.fromList vs) graph
    & Set.powerSet
    & Set.filter (\paths -> allFullEdges paths == Set.fromList (fullEdges graph))
    & bestVariants
      ( \paths ->
          Set.toList paths
            & concatMap toFullEdges
            & map fullEdgeData
            & foldl1 (<>)
      )
    & Set.map Set.toList
    & Set.toList