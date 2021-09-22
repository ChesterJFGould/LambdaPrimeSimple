module Lambda.Lower
( lower
)
where

import Compiler.Types
import Lambda.Types
import qualified Pred.Types as P

lower :: Program -> P.Program
lower (Program expr) = P.Program (lowerExpr expr)

lowerExpr :: TExpr -> P.Expr
lowerExpr (_, Value value) = P.Value (lowerValue value)
lowerExpr (_, BinOp (BNumOp op) l r) = P.NumOp op (lowerExpr l) (lowerExpr r)
lowerExpr expr@(typ, BinOp (BRelOp _) _ _) = lowerExpr (typ, If expr (typ, Value (typ, Bool True)) (typ, Value (typ, Bool False)))
lowerExpr (_, Apply f arg) = P.Apply (lowerExpr f) (lowerExpr arg)
lowerExpr (_, Lambda (_, aloc) body) = P.Lambda aloc (lowerExpr body)
lowerExpr (_, Let (_, aloc) val body) = P.Let aloc (lowerExpr val) (lowerExpr body)
lowerExpr (_, LetGlobal (_, label) val body) = P.LetGlobal label (lowerExpr val) (lowerExpr body)
lowerExpr (_, LetGlobals tLabels vals body) =
        let vals' = map lowerExpr vals
            body' = lowerExpr body
            labels = [ label | (_, label) <- tLabels ]
        in P.LetGlobals labels vals' body'
lowerExpr (_, If p c a) = lowerIf p (lowerExpr c) (lowerExpr a)

lowerValue :: TValue -> P.Value
lowerValue (_, Int i) = P.Int i
lowerValue (_, Bool b) = P.Bool b
lowerValue (_, Place p) = P.Place p

lowerIf :: TExpr -> P.Expr -> P.Expr -> P.Expr
lowerIf (_, Value (_, Bool True)) c a = c
lowerIf (_, Value (_, Bool False)) c a = a
lowerIf (_, BinOp (BRelOp op) l r) c a =
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
