module TestGraph where

import Data.Function ((&))
import Data.Set (Set)
import qualified Data.Set as Set
import Graph
  ( FullEdge,
    Graph (vertices),
    Vertex,
    fullEdgeData,
    fullEdges,
    toFullEdges, Route (EmptyRoute), RouteSet (getRoutes)
  )
import GraphMatrices ( allRoutesMatrix )
import Matrix (Matrix (rows))
import Semiring ( Semiring(zero, plus) )

availableRoutes :: (Ord e, Ord v) => Set (Vertex v) -> Graph e v -> Set (Route e v)
availableRoutes vs graph =
  zip [0, 1 ..] (graph & vertices)
    & filter (\(_, v) -> Set.member v vs)
    & concatMap (\(n, _) -> rows (allRoutesMatrix graph) !! n)
    & foldl plus zero
    & getRoutes
    & Set.filter (/= EmptyRoute)
    
bestVariants :: (Ord metric, Eq metric, Ord elem) => (elem -> metric) -> Set elem -> Set elem
bestVariants f elements =
  metrics
    & Set.filter (\(_, m) -> m == bestMetric)
    & Set.map fst
  where
    metrics = Set.map (\a -> (a, f a)) elements
    bestMetric = minimum $ Set.map snd metrics

allFullEdges :: (Ord v, Ord e) => Set (Route e v) -> Set (FullEdge e v)
allFullEdges routes = Set.fromList $ concatMap toFullEdges routes

bestRoutes :: (Semigroup e, Ord e, Ord v, Eq e, Eq v) => [Vertex v] -> Graph e v -> [[Route e v]]
bestRoutes vs graph =
  availableRoutes (Set.fromList vs) graph
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