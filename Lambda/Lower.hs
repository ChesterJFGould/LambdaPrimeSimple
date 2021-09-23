module Lambda.Lower
( lower
)
where

import Compiler.Types
import Lambda.Types
import qualified Pred.Types as P

lower :: Program -> P.Program
lower (Program expr) = P.Program (lowerExpr expr)

lowerExpr :: TExpr -> P.TExpr
lowerExpr (typ, Value value) = (lowerType typ, P.Value (lowerValue value))
lowerExpr (typ, BinOp (BNumOp op) l r) = (lowerType typ, P.NumOp op (lowerExpr l) (lowerExpr r))
lowerExpr expr@(typ, BinOp (BRelOp _) _ _) = lowerExpr (typ, If expr (typ, Value (typ, Bool True)) (typ, Value (typ, Bool False)))
lowerExpr (typ, Apply f arg) = (lowerType typ, P.Apply (lowerExpr f) (lowerExpr arg))
lowerExpr (typ, Lambda (alocType, aloc) body) = (lowerType typ, P.Lambda (lowerType alocType, aloc) (lowerExpr body))
lowerExpr (typ, Let (alocType, aloc) val body) = (lowerType typ, P.Let (lowerType alocType, aloc) (lowerExpr val) (lowerExpr body))
lowerExpr (typ, LetGlobal (labelType, label) val body) = (lowerType typ, P.LetGlobal (lowerType labelType, label) (lowerExpr val) (lowerExpr body))
lowerExpr (typ, LetGlobals tLabels vals body) =
        let vals' = map lowerExpr vals
            body' = lowerExpr body
            tLabels' = [ (lowerType labelType, label) | (labelType, label) <- tLabels ]
        in (lowerType typ, P.LetGlobals tLabels' vals' body')
lowerExpr (_, If p c a) = lowerIf p (lowerExpr c) (lowerExpr a)

lowerValue :: TValue -> P.TValue
lowerValue (typ, Int i) = (lowerType typ, P.Int i)
lowerValue (typ, Bool b) = (lowerType typ, P.Bool b)
lowerValue (typ, Place p) = (lowerType typ, P.Place p)

lowerIf :: TExpr -> P.TExpr -> P.TExpr -> P.TExpr
lowerIf (_, Value (_, Bool True)) c a = c
lowerIf (_, Value (_, Bool False)) c a = a
lowerIf (_, BinOp (BRelOp op) l r) c a@(typ, _) =
        let l' = lowerExpr l
            r' = lowerExpr r
        in ( typ
           , P.If (P.RelOp op l' r')
                  c
                  a
           )
lowerIf p c a@(typ, _) =
        let p' = lowerExpr p
        in ( typ
           , P.If (P.RelOp Eq p' (P.TBool, P.Value (P.TBool, P.Bool True)))
                  c
                  a
           )

lowerType :: Type -> P.Type
lowerType TInt = P.TInt
lowerType TBool = P.TBool
lowerType (TFunc from to) = P.TFunc (lowerType from) (lowerType to)
