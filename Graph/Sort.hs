module Graph.Sort
( sort
)
where

import Graph.Tarjan
import Graph.Types

import qualified Data.Array as A
import qualified Data.Map as M

sort :: (Show a, Ord a) => [(a, [a])] -> [[a]]
sort graph = let (graph', table) = encode graph
                 scc = tarjan graph'
             in decode scc table

encode :: (Show a, Ord a) => [(a, [a])] -> (Directed, A.Array Int a)
encode graph = let nodes = map fst graph
                   edges = map snd graph
                   encodings = M.fromList (zip nodes [0..])
                   edges' = map (map (encodings M.!)) edges
                   graph' = Directed (A.array (0, length graph - 1) (zip [0..] edges'))
                   table = A.array (0, length graph - 1) (zip [0..] nodes)
               in (graph', table)

decode :: Show a => SCC -> A.Array Int a -> [[a]]
decode (SCC scc) table = map (map (table A.!)) scc
