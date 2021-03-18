module Graph where

import Data.Function ((&))
import Data.Set (Set, union)
import qualified Data.Set as Set
import Matrix (Matrix (rows), buildMatrix, cols)
import RemoveCycles (removeCycles)
import Semiring (Semiring (..))

-- Vertex

newtype Vertex v = Vertex {vertex :: v}
  deriving (Eq, Ord)

instance (Show v) => Show (Vertex v) where
  show (Vertex v) = "(" ++ show v ++ ")"

-- Edge

data Edge e v = Edge {edgeData :: e, target :: Vertex v}
  deriving (Eq, Ord)

instance (Show e, Show v) => Show (Edge e v) where
  show edge =
    "-[" ++ show (edgeData edge) ++ "]->"
      ++ show (edge & target)

-- FullEdge

data FullEdge e v = FullEdge (Vertex v) (Edge e v)
  deriving (Eq, Ord)

instance (Show e, Show v) => Show (FullEdge e v) where
  show (FullEdge v edge) = show v ++ show edge

fullEdgeSource :: FullEdge e v -> Vertex v
fullEdgeSource (FullEdge v _) = v

fullEdgeTarget :: FullEdge e v -> Vertex v
fullEdgeTarget (FullEdge _ (Edge _ v)) = v

fullEdgeData :: FullEdge e v -> e
fullEdgeData (FullEdge _ (Edge e _)) = e

-- Route

newtype Route e v = Route [Edge e v]
  deriving (Eq, Ord)

instance (Show e, Show v) => Show (Route e v) where
  show (Route []) = " |-EMPTY_ROUTE-| "
  show (Route edges) = concatMap show edges

append :: Route e v -> Route e v -> Route e v
append (Route es1) (Route es2) = Route $ es1 ++ es2

simplify :: (Eq e, Eq v) => Route e v -> Route e v
simplify (Route edges) = Route $ removeCycles edges

-- RouteSet

newtype RouteSet e v = RouteSet (Set (Route e v))
  deriving (Eq)

fromRoutes :: (Ord e, Ord v) => [Route e v] -> RouteSet e v
fromRoutes = RouteSet . Set.fromList

instance (Show e, Show v) => Show (RouteSet e v) where
  show (RouteSet routes) = routes & Set.toList & concatMap (\route -> "| " ++ show route)

instance (Ord e, Ord v) => Semiring (RouteSet e v) where
  zero = RouteSet Set.empty
  one = RouteSet $ Set.singleton $ Route []
  plus (RouteSet rs1) (RouteSet rs2) = RouteSet $ rs1 `union` rs2
  prod (RouteSet rs1) (RouteSet rs2) =
    RouteSet $
      buildMatrix append (Set.toList rs1) (Set.toList rs2)
        & rows
        & concat
        & map simplify
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
    & concatMap (\v -> map (FullEdge v) $ edgeFunc g v)
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

-- Path

data Path e v = Path {source :: Vertex v, route :: Route e v}
  deriving (Eq, Ord)

instance (Show e, Show v) => Show (Path e v) where
  show (Path v (Route [])) = show v
  show path = (source path & show) ++ (route path & show)

toPath :: Vertex v -> Route e v -> Path e v
toPath = Path

toPaths :: Vertex v -> RouteSet e v -> [Path e v]
toPaths v (RouteSet set) =
  Set.toList set
    & map (Path v)

toFullEdges :: Path e v -> [FullEdge e v]
toFullEdges (Path src (Route edges)) =
  zipWith FullEdge
    (src : map target edges)
    edges