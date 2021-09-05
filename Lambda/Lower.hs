module Lambda.Lower
( lower
)
where

import Compiler.Types
import Lambda.Types
import qualified Pred.Types as P

lower :: Program -> P.Program
lower (Program expr) = P.Program (lowerExpr expr)

lowerExpr :: Expr -> P.Expr
lowerExpr (Value value) = P.Value (lowerValue value)
lowerExpr (BinOp (BNumOp op) l r) = P.NumOp op (lowerExpr l) (lowerExpr r)
lowerExpr expr@(BinOp (BRelOp _) _ _) = lowerExpr (If expr (Value (Bool True)) (Value (Bool False)))
lowerExpr (Apply f arg) = P.Apply (lowerExpr f) (lowerExpr arg)
lowerExpr (Lambda aloc body) = P.Lambda aloc (lowerExpr body)
lowerExpr (Let aloc val body) = P.Let aloc (lowerExpr val) (lowerExpr body)
lowerExpr (LetGlobal label val body) = P.LetGlobal label (lowerExpr val) (lowerExpr body)
lowerExpr (LetGlobals labels vals body) =
        let vals' = map lowerExpr vals
            body' = lowerExpr body
        in P.LetGlobals labels vals' body'
lowerExpr (If p c a) = lowerIf p (lowerExpr c) (lowerExpr a)

lowerValue :: Value -> P.Value
lowerValue (Int i) = P.Int i
lowerValue (Bool b) = P.Bool b
lowerValue (Place p) = P.Place p

lowerIf :: Expr -> P.Expr -> P.Expr -> P.Expr
lowerIf (Value (Bool True)) c a = c
lowerIf (Value (Bool False)) c a = a
lowerIf (BinOp (BRelOp op) l r) c a =
        let l' = lowerExpr l
            r' = lowerExpr r
        in P.If (P.RelOp op l' r')
                c
                a
lowerIf p c a =
        let p' = lowerExpr p
        in P.If (P.RelOp Eq p' (P.Value (P.Bool True)))
                c
                a
