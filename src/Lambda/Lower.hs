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
lowerExpr (typ, Atom atom) = (lowerType typ, P.Atom (lowerAtom atom))
lowerExpr (typ, BinOp (Num op) l r) = (lowerType typ, P.NumOp op (lowerExpr l) (lowerExpr r))
lowerExpr pred@(typ, BinOp (Rel _) _ _) =
        let typ' = lowerType typ
        in (typ', lowerIf pred
                          (typ', P.Atom (typ', P.Bool True))
                          (typ', P.Atom (typ', P.Bool False)))
lowerExpr (typ, Apply f arg) = (lowerType typ, P.Apply (lowerExpr f) (lowerExpr arg))
lowerExpr (typ, Lambda aloc body) = (lowerType typ, P.Lambda (lowerAloc aloc) (lowerExpr body))
lowerExpr (typ, Let aloc val body) =
        (lowerType typ, P.Let (lowerAloc aloc) (lowerExpr val) (lowerExpr body))
lowerExpr (typ, LetRec alocs vals body) =
        (lowerType typ, P.LetRec (map lowerAloc alocs) (map lowerExpr vals) (lowerExpr body))
lowerExpr (typ, If p c a) = (lowerType typ, lowerIf p (lowerExpr c) (lowerExpr a))

lowerIf :: TExpr -> P.TExpr -> P.TExpr -> P.Expr
lowerIf (_, Atom (_, Bool True)) (_, c) _ = c
lowerIf (_, Atom (_, Bool False)) _ (_, a) = a
lowerIf (_, BinOp (Rel op) l r) c a = P.If (P.RelOp op (lowerExpr l) (lowerExpr r))
                                           c
                                           a
lowerIf p c a = P.If (P.RelOp Eq (lowerExpr p) (P.TBool, P.Atom (P.TBool, P.Bool True)))
                     c
                     a

lowerAtom :: TAtom -> P.TAtom
lowerAtom (typ, Int i) = (lowerType typ, P.Int i)
lowerAtom (typ, Bool b) = (lowerType typ, P.Bool b)
lowerAtom (typ, AtAloc aloc) = (lowerType typ, P.AtAloc (lowerAloc aloc))

lowerType :: Type -> P.Type
lowerType TInt = P.TInt
lowerType TBool = P.TBool
lowerType (TFunc f t) = P.TFunc (lowerType f) (lowerType t)

lowerAloc :: TAloc -> P.TAloc
lowerAloc (typ, aloc) = (lowerType typ, aloc)
