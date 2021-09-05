module CPSAsm.Lower
( lower
)
where

import Compiler.Types
import CPSAsm.Types
import qualified CPSFree.Types as F

import qualified Data.Set.Ordered as O

lower :: Program -> F.Program
lower (Program tuples blocks body) = F.Program tuples (lowerBlocks blocks) (lowerBody body)

lowerBlocks :: [Block] -> [F.Block]
lowerBlocks = map lowerBlock

lowerBlock :: Block -> F.Block
lowerBlock (Block label body) = F.Block label (lowerBody body)

lowerBody :: Body -> F.Body
lowerBody (Body initialized expr) = F.Body initialized (lowerExpr expr)

lowerExpr :: Expr -> F.TExpr
lowerExpr (Set loc triv body) =
        let body'@(freeOut, _) = lowerExpr body
            freeIn = freeTriv triv O.|<> (O.delete loc freeOut)
        in (freeIn, F.Set loc triv body')
lowerExpr (NumOp op l r body) =
        let body'@(freeOut, _) = lowerExpr body
            freeIn = l O.|< (r O.|< freeOut)
        in (freeIn, F.NumOp op l r body')
lowerExpr (MRef loc place offset body) =
        let body'@(freeOut, _) = lowerExpr body
            freeIn = freePlace place O.|<> (O.delete loc freeOut)
        in (freeIn, F.MRef loc place offset body')
lowerExpr (MSet ptr offset val body) =
        let body'@(freeOut, _) = lowerExpr body
            freeIn = freePlace ptr O.|<> (freePlace val O.|<> freeOut)
        in (freeIn, F.MSet ptr offset val body')
lowerExpr (If (RelOp op l r) c a) =
        let c'@(freeOutC, _) = lowerExpr c
            a'@(freeOutA, _) = lowerExpr a
            freeOut = freeOutC O.|<> freeOutA
            freeIn = l O.|< (r O.|< freeOut)
        in (freeIn, F.If (F.RelOp op l r) c' a')
lowerExpr (Jump place used) =
        let freeIn = freePlace place O.|<> O.fromList (map MReg used)
        in (freeIn, F.Jump place used)

freeTriv :: MTriv -> O.OSet Mloc
freeTriv (MPlace place) = freePlace place
freeTriv (MWord _) = O.empty

freePlace :: MPlace -> O.OSet Mloc
freePlace (MMloc loc) = O.singleton loc
freePlace (MLabel _) = O.empty
