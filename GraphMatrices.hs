module GraphMatrices where

import Data.Function ((&))
import Graph
    ( Graph(edgeFunc, vertices),
      RouteSet,
      Route(Route),
      Edge(Edge, edgeTarget, edgeData),
      Vertex(Vertex, vertex),
      fromRoutes )
import Matrix (Matrix (..), buildMatrix)
import Semiring (Semiring (one, plus, prod))

graphMatrix :: (Eq v) => (v -> v -> [e] -> x) -> Graph e v -> Matrix x
graphMatrix f digraph =
  buildMatrix
    ( \v1 v2 ->
        edgeFunc digraph v1
          & filter (\edge -> edgeTarget edge == v2)
          & map edgeData
          & f (vertex v1) (vertex v2)
    )
    (vertices digraph)
    (vertices digraph)

adjacencyMatrix :: (Eq v) => Graph e v -> Matrix Bool
adjacencyMatrix = plus one . graphMatrix (\_ _ es -> not $ null es)

edgeDataMatrix :: (Eq v) => Graph e v -> Matrix [e]
edgeDataMatrix = graphMatrix (\_ _ es -> es)

primitiveRouteSetMatrix :: (Eq v, Eq e, Ord e, Ord v) => Graph e v -> Matrix (RouteSet e v)
primitiveRouteSetMatrix =
  graphMatrix
    ( \v1 v2 es ->
        es
          & map
            ( \eData ->
                Edge eData (Vertex v2)
                  & (: [])
                  & Route (Vertex v1)
            )
          & fromRoutes
    )

allRoutesMatrix :: (Eq v, Eq e, Ord e, Ord v) => Graph e v -> Matrix (RouteSet e v)
allRoutesMatrix graph =
  primitiveRouteSetMatrix graph
    & plus one
    & iterate (\a -> a `prod` a)
    & firstRepeatedElement
  where
    firstRepeatedElement (x1 : x2 : xs) =
      if x1 == x2
        then x1
        else firstRepeatedElement (x2 : xs)