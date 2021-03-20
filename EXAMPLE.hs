module EXAMPLE where

import Graph (Graph, Vertex (Vertex), buildGraph)
import TestGraph (bestPaths)

newtype Ed = Ed Int
  deriving (Ord, Eq)

instance Semigroup Ed where
  (Ed a) <> (Ed b) = Ed $ a + b

instance Show Ed where
  show (Ed a) = show a

testGraph :: Graph Ed Integer
testGraph =
  buildGraph
    [ (1, [(Ed 1, 2)]),
      (2, [(Ed 1, 3), (Ed 1, 6)]),
      (3, [(Ed 1, 5), (Ed 1, 4)]),
      (4, [(Ed 1, 2)])
    ]

showLst :: (Show a) => [a] -> String
showLst = concatMap (\a -> '\n' : show a)

main :: IO ()
main = do
  putStrLn $ concatMap (('\n' :) . showLst) (bestPaths [Vertex 1] testGraph)
  putStrLn ""
