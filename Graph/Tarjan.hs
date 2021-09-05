module Graph.Tarjan
( tarjan
)
where

import Graph.Types

import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Array as A
import qualified Data.IntMap as IM

data TarjanState = TS { indices :: IM.IntMap Int
                      , lowlinks :: IM.IntMap Int
                      , index :: Int
                      , stack :: [Int]
                      , onStack :: IM.IntMap ()
                      }
                 deriving Show

emptyTarjanState :: TarjanState
emptyTarjanState = TS IM.empty IM.empty 0 [] IM.empty

type Tarjan a = WriterT [[Int]] (State TarjanState) a

execTarjan :: Tarjan a -> [[Int]]
execTarjan computation = evalState (execWriterT computation) emptyTarjanState

-- Takes in a graph and produces its strongly connected components in
-- topological order.
tarjan :: Directed -> SCC
tarjan graph@(Directed edges) = SCC (execTarjan (mapM (maybeStrongConnect graph) (A.indices edges)))

maybeStrongConnect :: Directed -> Int -> Tarjan ()
maybeStrongConnect graph node = do
                                index <- getIndex node
                                case index of
                                     Just _ -> return ()
                                     Nothing -> strongConnect graph node

strongConnect :: Directed -> Int -> Tarjan ()
strongConnect graph@(Directed edges) node = do
                                            index <- nextIndex
                                            setIndex node index
                                            setLowlink node index
                                            push node
                                            let neighbours = edges A.! node
                                            mapM (consider graph node) neighbours
                                            lowlink <- getLowlink node
                                            if lowlink /= index
                                            then return ()
                                            else newComponent node

consider :: Directed -> Int -> Int -> Tarjan ()
consider graph node neighbour = do
                                neighbourIndex <- getIndex neighbour
                                neighbourOnStack<- isOnStack neighbour
                                case (neighbourIndex, neighbourOnStack) of
                                     (Nothing, _) -> do
                                                strongConnect graph neighbour
                                                nodeLowlink <- getLowlink node
                                                neighbourLowlink <- getLowlink neighbour
                                                setLowlink node (min nodeLowlink neighbourLowlink)
                                     (Just neighbourIndex', True) -> do
                                                                     nodeLowlink <- getLowlink node
                                                                     setLowlink node (min nodeLowlink neighbourIndex')
                                     _ -> return ()

newComponent :: Int -> Tarjan ()
newComponent = newComponent' []

newComponent' :: [Int] -> Int -> Tarjan ()
newComponent' acc root = do
                         node <- pop
                         if node /= root
                         then newComponent' (node : acc) root
                         else tell [ root : acc ]

nextIndex :: Tarjan Int
nextIndex = do
            state <- lift get
            let currentIndex = index state
            lift (put (state { index = currentIndex + 1 }))
            return currentIndex

getIndex :: Int -> Tarjan (Maybe Int)
getIndex node = do
                state <- lift get
                return (IM.lookup node (indices state))

setIndex :: Int -> Int -> Tarjan ()
setIndex node index = do
                      state <- lift get
                      lift (put (state { indices = IM.insert node index (indices state) }))

getLowlink :: Int -> Tarjan Int
getLowlink node = do
                  state <- lift get
                  return ((lowlinks state) IM.! node)

setLowlink :: Int -> Int -> Tarjan ()
setLowlink node lowlink = do
                          state <- lift get
                          lift (put (state { lowlinks = IM.insert node lowlink (lowlinks state) }))

push :: Int -> Tarjan ()
push node = do
             state <- lift get
             lift (put (state { stack = node : stack state
                              , onStack = IM.insert node () (onStack state)
                              }))

pop :: Tarjan Int
pop = do
      state <- lift get
      let node = head (stack state)
      lift (put (state { stack = tail (stack state)
                       , onStack = IM.delete node (onStack state)
                       }))
      return node

isOnStack :: Int -> Tarjan Bool
isOnStack node = do
                 state <- lift get
                 case IM.lookup node (onStack state) of
                      Nothing -> return False
                      Just () -> return True
