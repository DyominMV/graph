module Graph where

import Data.Foldable (minimumBy)
import Data.Function ((&))
import Data.List (groupBy)
import Data.Maybe (catMaybes)
import Data.Set (Set, union)
import qualified Data.Set as Set
import Matrix (Matrix (rows), buildMatrix, cols)
import Semiring (Semiring (..))

-- Vertex

newtype Vertex v = Vertex {vertex :: v}
  deriving (Eq, Ord)

instance (Show v) => Show (Vertex v) where
  show (Vertex v) = "(" ++ show v ++ ")"

-- Edge

data Edge e v = Edge
  { edgeData :: e,
    edgeTarget :: Vertex v
  }
  deriving (Eq, Ord)

instance (Show e, Show v) => Show (Edge e v) where
  show edge =
    "-[" ++ show (edgeData edge) ++ "]->"
      ++ show (edge & edgeTarget)

-- FullEdge

data FullEdge e v = FullEdge
  { fullEdgeSource :: Vertex v,
    fullEdgeData :: e,
    fullEdgeTarget :: Vertex v
  }
  deriving (Eq, Ord)

instance (Show e, Show v) => Show (FullEdge e v) where
  show (FullEdge v1 e v2) = show v1 ++ show (Edge e v2)

-- Route

data Route e v
  = Route
      { routeSource :: Vertex v,
        routeEdges :: [Edge e v]
      }
  | EmptyRoute
  deriving (Eq, Ord)

instance (Show e, Show v) => Show (Route e v) where
  show EmptyRoute = " |-No moving-| "
  show (Route src []) = " |-Standing at " ++ show src ++ "-| "
  show (Route src edges) = show src ++ concatMap show edges

routeTarget :: Route e v -> Vertex v
routeTarget (Route _ edges) = edges & last & edgeTarget

append :: Eq v => Route e v -> Route e v -> Maybe (Route e v)
append EmptyRoute r2 = Just r2
append r1 EmptyRoute = Just r1
append r1@(Route src1 edges1) r2@(Route src2 edges2) =
  if routeTarget r1 == routeSource r2
    then Just (Route src1 (edges1 ++ edges2))
    else Nothing

toFullEdges :: Route e v -> [FullEdge e v]
toFullEdges EmptyRoute = []
toFullEdges (Route src edges) =
  zipWith3
    FullEdge
    (src : map edgeTarget edges)
    (map edgeData edges)
    (map edgeTarget edges)

fullEdgeSet :: (Ord v, Ord e) => Route e v -> Set (FullEdge e v)
fullEdgeSet = Set.fromList . toFullEdges

toRoute :: [FullEdge e v] -> Route e v
toRoute [] = EmptyRoute
toRoute fullEdges =
  Route
    (head fullEdges & fullEdgeSource)
    (fullEdges & map (\(FullEdge _ e v2) -> Edge e v2))

-- RouteSet

newtype RouteSet e v = RouteSet {getRoutes :: Set (Route e v)}
  deriving (Eq)

fromRoutes :: (Ord e, Ord v) => [Route e v] -> RouteSet e v
fromRoutes = RouteSet . Set.fromList

instance (Show e, Show v) => Show (RouteSet e v) where
  show (RouteSet routes) = routes & Set.toList & concatMap (\route -> "| " ++ show route)

instance (Ord e, Ord v) => Semiring (RouteSet e v) where
  zero = RouteSet Set.empty
  one = RouteSet $ Set.singleton EmptyRoute
  plus (RouteSet rs1) (RouteSet rs2) =
    RouteSet $
      (rs1 `union` rs2)
        & Set.toList
        & groupOn fullEdgeSet
        & map (minimumOn (length . toFullEdges))
        & Set.fromList
    where
      groupOn :: (Eq b) => (a -> b) -> [a] -> [[a]]
      groupOn _ [] = []
      groupOn f (x : xs) = (x : filter ((== f x) . f) xs) : groupOn f (filter ((/= f x) . f) xs)
      minimumOn :: (Ord b) => (a -> b) -> [a] -> a
      minimumOn f = minimumBy (\a b -> f a `compare` f b)
  prod (RouteSet rs1) (RouteSet rs2) =
    RouteSet $
      buildMatrix append (Set.toList rs1) (Set.toList rs2)
        & rows
        & concat
        & catMaybes
        & Set.fromList

-- Graph

data Graph e v = Digraph
  { vertices :: [Vertex v],
    edgeFunc :: Vertex v -> [Edge e v]
  }

edges :: (Ord e, Ord v) => Graph e v -> [Edge e v]
edges g =
  vertices g
    & concatMap (edgeFunc g)
    & Set.fromList
    & Set.toList

fullEdges :: (Ord e, Ord v) => Graph e v -> [FullEdge e v]
fullEdges g =
  vertices g
    & concatMap (\v -> map (\(Edge e v2) -> FullEdge v e v2) $ edgeFunc g v)
    & Set.fromList
    & Set.toList

buildGraph :: (Eq v, Ord v) => [(v, [(e, v)])] -> Graph e v
buildGraph lst = Digraph foundVertices edgeExtracter
  where
    foundVertices =
      (++)
        (map (Vertex . fst) lst)
        (map (Vertex . snd) (lst & concatMap snd))
        & Set.fromList
        & Set.toList

    edgeExtracter (Vertex v) =
      lst
        & filter ((== v) . fst)
        & concatMap snd
        & map (\(e, v) -> Edge e $ Vertex v)