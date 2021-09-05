module CPSRegisters.Lower
( lower
)
where

import Compiler.Gensym
import Compiler.Types
import CPSRegisters.Types
import qualified S64.Types as S

lower :: Program -> Gensym S.Program
lower (Program tuples blocks body) =
        do
        blockStmts <- lowerBlocks blocks
        bodyStmts <- lowerBody body
        return (S.Program tuples (bodyStmts ++ blockStmts))

lowerBlocks :: [Block] -> Gensym [S.Stmt]
lowerBlocks blocks =
        do
        blocks' <- mapM lowerBlock blocks
        return (concat blocks')

lowerBlock :: Block -> Gensym [S.Stmt]
lowerBlock (Block label body) =
        do
        body' <- lowerBody body
        return (S.Labelled label (head body') : (tail body'))

lowerBody :: Body -> Gensym [S.Stmt]
lowerBody (Body expr) = lowerExpr expr

lowerExpr :: Expr -> Gensym [S.Stmt]
lowerExpr (Set reg triv body) = (S.Set reg triv :) <$>  lowerExpr body
lowerExpr (NumOp op l r body) = (S.NumOp op l r :) <$> lowerExpr body
lowerExpr (MRef reg ptr offset body) = (S.MRef reg ptr offset :) <$> lowerExpr body
lowerExpr (MSet ptr offset val body) = (S.MSet ptr offset val :) <$> lowerExpr body
lowerExpr (If (RelOp op l r) c a) =
        do
        c' <- lowerExpr c
        a' <- lowerExpr a
        cLabel <- genLabel "consequence"
        let c'' = S.Labelled cLabel (head c') : (tail c')
        return ( concat [ [ S.Compare l r
                          , S.JumpIf op cLabel
                          ]
                        , a'
                        , c''
                        ]
               )
lowerExpr (Jump place) = return [ S.Jump place ]
