module Graph.Types where

import Data.Array

newtype Directed = Directed (Array Int [Int])

newtype SCC = SCC [[Int]]
