module Graph.Colour
( colour
, Node (..)
)
where

import qualified Data.Map as M
import qualified Data.Set as S

import Data.Maybe

data Node v c = Node v
              | Colour c
              deriving (Eq, Ord)

colour :: (Ord v, Ord c) => S.Set v -> S.Set (Node v c, Node v c) -> [c] -> M.Map v c
colour nodes edges colours = foldr (assignColor edges colours) M.empty nodes

assignColor :: (Ord v, Ord c) => S.Set (Node v c, Node v c) -> [c] -> v -> M.Map v c -> M.Map v c
assignColor edges colours node assignments = let nodeNeighbours = neighbours node edges
                                                 neighbourColours = ( S.map fromJust
                                                                    . S.filter isJust
                                                                    . S.map (nodeToColour assignments)
                                                                    ) nodeNeighbours
                                                 nodeColour = mex colours neighbourColours
                                                 assignments' = M.insert node nodeColour assignments
                                             in assignments'

neighbours :: (Ord v, Ord c) => v -> S.Set (Node v c, Node v c) -> S.Set (Node v c)
neighbours node edges = ( S.map fromJust
                        . S.filter isJust
                        . S.map (containsThenOther node)
                        ) edges

containsThenOther :: (Eq v, Eq c) => v -> (Node v c, Node v c) -> Maybe (Node v c)
containsThenOther node (a, b)
                  | Node node == a = Just b
                  | Node node == b = Just a
                  | otherwise = Nothing

nodeToColour :: (Ord v, Ord c) => M.Map v c -> Node v c -> Maybe c
nodeToColour assignments (Node node) = M.lookup node assignments
nodeToColour assignments (Colour colour) = Just colour

mex :: Ord c => [c] -> S.Set c -> c
mex (colour : rest) conflicts
    | colour `S.member` conflicts = mex rest conflicts
    | otherwise = colour
