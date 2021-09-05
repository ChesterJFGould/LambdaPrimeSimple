module CPSFree.Lower
( lower
)
where

import Compiler.Gensym
import qualified Compiler.Locations as L
import Compiler.Types
import CPSFree.Conflicts
import CPSFree.Types
import qualified CPSRegisters.Types as R
import qualified Graph.Colour as G

import qualified Data.Map as M
import qualified Data.Set as S

data Assignment = Reg Reg
                | Spill Int
                deriving (Eq, Ord)

type Assignments = M.Map Aloc Assignment

lower :: Program -> R.Program
lower (Program tuples blocks body) = R.Program tuples (lowerBlocks blocks) (lowerBody body)

lowerBlocks :: [Block] -> [R.Block]
lowerBlocks = map lowerBlock

lowerBlock :: Block -> R.Block
lowerBlock (Block label body) = R.Block label (lowerBody body)

lowerBody :: Body -> R.Body
lowerBody body@(Body _ expr) =
        let (nodes, edges) = conflicts body
            assignmentLocs = map Reg L.graphColourRegisters
                             ++
                             map Spill [0..]
            edges' = S.map locsToNodes edges
            assignments = G.colour nodes edges' assignmentLocs
            expr' = lowerExpr assignments expr
            spillSize = numSpillLocations assignments * 8
        in R.Body (R.Set L.frameRegister (RPlace (RReg L.heapRegister))
                         (R.Set L.tempRegister1 (RWord (fromIntegral spillSize))
                                (R.NumOp Add L.heapRegister L.tempRegister1 expr')))

locsToNodes :: (Mloc, Mloc) -> (G.Node Aloc Assignment, G.Node Aloc Assignment)
locsToNodes (a, b) = (locToNode a, locToNode b)

locToNode :: Mloc -> G.Node Aloc Assignment
locToNode (MAloc aloc) = G.Node aloc
locToNode (MReg reg) = G.Colour (Reg reg)

numSpillLocations :: Assignments -> Int
numSpillLocations assignments = numSpillLocations' 0 (map snd (M.toList assignments))

numSpillLocations' :: Int -> [Assignment] -> Int
numSpillLocations' maxLoc [] = maxLoc
numSpillLocations' maxLoc (Reg _ : rest) = numSpillLocations' maxLoc rest
numSpillLocations' maxLoc (Spill loc : rest) = numSpillLocations' (max maxLoc (loc + 1)) rest

lowerExpr :: Assignments -> TExpr -> R.Expr
lowerExpr assignments (_, Set (MAloc aloc) triv body) =
        let (triv', wrapper) = lowerTriv assignments L.tempRegister1 triv
            body' = lowerExpr assignments body
        in case assignments M.! aloc of
                Reg reg -> wrapper ( R.Set reg triv' body')
                Spill offset -> wrapper ( R.Set L.tempRegister1 triv'
                                                (spill (RReg L.tempRegister1) offset body')
                                        )
lowerExpr assignments (_, Set (MReg reg) triv body) =
        let (triv', wrapper) = lowerTriv assignments L.tempRegister1 triv
            body' = lowerExpr assignments body
        in wrapper (R.Set reg triv' body')
lowerExpr assignments (_, NumOp op l@(MAloc aloc) r body) =
        let (r', rWrapper) = lowerLoc assignments L.tempRegister1 r
            (l', lWrapper) = lowerLoc assignments L.tempRegister2 l
            body' = lowerExpr assignments body
        in case assignments M.! aloc of
                Reg _ -> rWrapper (lWrapper (R.NumOp op l' r' body'))
                Spill offset -> rWrapper (lWrapper (R.NumOp op l' r'
                                                            (spill (RReg L.tempRegister2) offset body')))
lowerExpr assignments (_, NumOp op (MReg reg) r body) =
        let (r', wrapper) = lowerLoc assignments L.tempRegister1 r
            body' = lowerExpr assignments body
        in wrapper (R.NumOp op reg r' body')
lowerExpr assignments (_, MRef (MAloc aloc) ptr offset body) =
        let (ptr', wrapper) = lowerPlace assignments L.tempRegister1 ptr
            body' = lowerExpr assignments body
        in case assignments M.! aloc of
           Reg reg -> wrapper (R.MRef reg ptr' offset body')
           Spill spillOffset  -> wrapper ( R.MRef L.tempRegister1 ptr' offset
                                                  (spill (RReg L.tempRegister1) spillOffset body')
                                         )
lowerExpr assignments (_, MRef (MReg reg) ptr offset body) =
        let (ptr', wrapper) = lowerPlace assignments L.tempRegister1 ptr
            body' = lowerExpr assignments body
        in wrapper (R.MRef reg ptr' offset body')
lowerExpr assignments (_, MSet ptr offset val body) =
        let (ptr', ptrWrapper) = lowerPlace assignments L.tempRegister1 ptr
            (val', valWrapper) = lowerPlace assignments L.tempRegister2 val
            body' = lowerExpr assignments body
        in ptrWrapper (valWrapper (R.MSet ptr' offset val' body'))
lowerExpr assignments (_, If (RelOp op l r) c a) =
        let (l', lWrapper) = lowerLoc assignments L.tempRegister1 l
            (r', rWrapper) = lowerLoc assignments L.tempRegister2 r
            c' = lowerExpr assignments c
            a' = lowerExpr assignments a
        in lWrapper (rWrapper (R.If (R.RelOp op l' r') c' a'))
lowerExpr assignments (_, Jump place _) =
        let (place', wrapper) = lowerPlace assignments L.tempRegister1 place
        in wrapper (R.Jump place')

lowerLoc :: Assignments -> Reg -> Mloc -> (Reg, R.Expr -> R.Expr)
lowerLoc _ _ (MReg reg) = (reg, id)
lowerLoc assignments tmp (MAloc aloc) =
        case assignments M.! aloc of
             Reg reg -> (reg, id)
             Spill offset -> (tmp, unspill tmp offset)

lowerPlace :: Assignments -> Reg -> MPlace -> (RPlace, R.Expr -> R.Expr)
lowerPlace assignments tmp (MMloc loc) =
        let (loc', wrapper) = lowerLoc assignments tmp loc
        in (RReg loc', wrapper)
lowerPlace _ _ (MLabel label) = (RLabel label, id)

lowerTriv :: Assignments -> Reg -> MTriv -> (RTriv, R.Expr -> R.Expr)
lowerTriv assignments tmp (MPlace place) =
        let (place', wrapper) = lowerPlace assignments tmp place
        in (RPlace place', wrapper)
lowerTriv _ _ (MWord word) = (RWord word, id)

spill :: RPlace -> Int -> R.Expr -> R.Expr
spill place offset body = R.MSet (RReg L.frameRegister) (offset * 8) place body

unspill :: Reg -> Int -> R.Expr -> R.Expr
unspill reg offset body = R.MRef reg (RReg L.frameRegister) (offset * 8) body
