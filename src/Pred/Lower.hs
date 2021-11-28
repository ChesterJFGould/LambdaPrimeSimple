{-# LANGUAGE BlockArguments #-}
module Pred.Lower
( lower
)
where

import Compiler.Gensym
import Compiler.Types

import qualified CPS.Types as C
import Pred.Types

import Control.Monad.State
import qualified Data.Map as M

haltCont :: C.TAloc -> Env C.Expr
haltCont resultAloc =
        do
        let haltType = C.TCont C.TInt
        haltAloc <- genTAloc haltType "halt"
        return (C.Let haltAloc (C.AtLabel (haltType, HaltLabel))
                      (C.CallCont haltAloc resultAloc))

type Env = StateT (M.Map Aloc C.TAloc) Gensym

evalEnv :: Env a -> Gensym a
evalEnv computation = evalStateT computation M.empty

lower :: Program -> Gensym C.Program
lower (Program expr) = C.Program <$> evalEnv (lowerExpr expr haltCont)

lowerExpr :: TExpr -> (C.TAloc -> Env C.Expr) -> Env C.Expr
lowerExpr (_, Atom atom) k = lowerAtom atom k
lowerExpr (typ, NumOp op l r) k =
        lowerExpr l \lAloc ->
        lowerExpr r \rAloc ->
        do
        resultAloc <- genTAloc (lowerType typ) "numOpRes"
        body <- k resultAloc
        return (C.Let resultAloc (C.NumOp op lAloc rAloc) body)
lowerExpr (typ, Apply f arg) k =
        lowerExpr f \fAloc ->
        lowerExpr arg \argAloc ->
        do
        let typ' = lowerType typ
        resultAloc <- genTAloc typ' "funRes"
        contBody <- k resultAloc
        contAloc <- genTAloc (C.TCont typ') "cont"
        return (C.LetCont contAloc (C.Cont resultAloc contBody)
                          (C.CallFunc fAloc contAloc argAloc))
lowerExpr func@(typ, (Lambda _ _)) k =
        do
        func' <- lowerFunc func
        let typ' = lowerType typ
        funcAloc <- genTAloc typ' "func"
        letBody <- k funcAloc
        return (C.LetRecFuncs [funcAloc] [func'] letBody)
lowerExpr (typ, Let aloc val body) k =
        lowerExpr val \valAloc ->
        do
        putAlocAlias aloc valAloc
        lowerExpr body k
lowerExpr (typ, LetRec alocs funcs body) k =
        do
        let alocs' = map lowerAloc alocs
        funcs' <- mapM lowerFunc funcs
        body' <- lowerExpr body k
        return (C.LetRecFuncs alocs' funcs' body')
lowerExpr (typ, If p c a) k =
        lowerPred p \p' ->
        do
        let typ' = lowerType typ
        joinAloc <- genTAloc (C.TCont typ') "join"
        joinResultAloc <- genTAloc typ' "joinResult"
        joinBody <- k joinResultAloc
        let joinCont resultAloc = return (C.CallCont joinAloc resultAloc)
        c' <- lowerExpr c joinCont
        a' <- lowerExpr a joinCont
        return (C.LetCont joinAloc (C.Cont joinResultAloc joinBody)
                          (C.If p' c' a'))

lowerPred :: Pred -> (C.Pred -> Env C.Expr) -> Env C.Expr
lowerPred (RelOp op l r) k =
        lowerExpr l \lAloc ->
        lowerExpr r \rAloc ->
        k (C.RelOp op lAloc rAloc)

-- Should fail if called with non-function argument because we haven't proven
-- that LetRec only contains lambdas yet.
lowerFunc :: TExpr -> Env C.Func
lowerFunc (typ, Lambda arg body) =
        do
        let typ'@(C.TFunc contTyp _) = lowerType typ
        contAloc <- genTAloc contTyp "contArg"
        let arg' = lowerAloc arg
        body' <- lowerExpr body (\resultAloc -> return (C.CallCont contAloc resultAloc))
        return (C.Func contAloc arg' body')

lowerAtom :: TAtom -> (C.TAloc -> Env C.Expr) -> Env C.Expr
lowerAtom (typ, Int i) k =
        do
        let typ' = lowerType typ
        intAloc <- genTAloc typ' "int"
        letBody <- k intAloc
        return (C.Let intAloc (C.Int i) letBody)
lowerAtom (typ, Bool b) k =
        do
        let typ' = lowerType typ
        boolAloc <- genTAloc typ' "bool"
        letBody <- k boolAloc
        return (C.Let boolAloc (C.Bool b) letBody)
lowerAtom (typ, AtAloc tAloc@(_, aloc)) k =
        do
        env <- get
        let maybeTAloc' = M.lookup aloc env
        k (maybe (lowerAloc tAloc) id maybeTAloc')

lowerType :: Type -> C.Type
lowerType TInt = C.TInt
lowerType TBool = C.TBool
lowerType (TFunc f t) = C.TFunc (C.TCont (lowerType t)) (lowerType f)

lowerAloc :: TAloc -> C.TAloc
lowerAloc (typ, aloc) = (lowerType typ, aloc)

putAlocAlias :: TAloc -> C.TAloc -> Env ()
putAlocAlias (_, aloc) alias =
        do
        env <- get
        let env' = M.insert aloc alias env
        put env'

genTAloc :: C.Type -> String -> Env C.TAloc
genTAloc typ template =
        do
        aloc <- lift (genAloc template)
        return (typ, aloc)
