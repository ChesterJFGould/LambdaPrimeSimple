{-# LANGUAGE BlockArguments #-}
module Globals.Lower
( lower
)
where

import Compiler.Gensym
import Compiler.Types
import qualified CPS.Types as C
import Globals.Types

import Control.Monad.State
import qualified Data.Map as M

type Env a = StateT (M.Map Aloc C.TAloc) (Gensym) a

halt :: C.TAPlace
halt = (C.TCont C.TInt, ALabel HaltLabel)

evalEnv :: Env a -> Gensym a
evalEnv computation = evalStateT computation M.empty

lower :: Program -> Gensym C.Program
lower (Program expr) = evalEnv (C.Program <$> lowerTail expr halt)

lowerTail :: TExpr -> C.TAPlace -> Env C.Expr
lowerTail (_, Value value) k = lowerValue value (\(vAlocType, vAloc) -> return (C.CallCont k (vAlocType, (AAloc vAloc))))
lowerTail expr@(_, NumOp _ _ _) k =
        lowerExpr expr \(resultAlocType, resultAloc) ->
        return (C.CallCont k (resultAlocType, (AAloc resultAloc)))
lowerTail (_, Apply f arg) k =
        lowerExprs [f, arg] \[(fAlocType, fAloc), (argAlocType, argAloc)] ->
        do
        let fPlace = (fAlocType, AAloc fAloc)
            argPlace = (argAlocType, AAloc argAloc)
        return (C.CallFunc fPlace k argPlace)
lowerTail expr@(_, TupleRef _ _) k =
        lowerExpr expr \(refAlocType, refAloc) ->
        return (C.CallCont k (refAlocType, (AAloc refAloc)))
lowerTail (_, Let (_, aloc) val body) k =
        lowerExpr val \(valAlocType, valAloc) ->
        do
        putAloc aloc valAloc
        lowerTail body k
lowerTail (_, LetFunc (alocType, aloc) func body) k =
        do
        func' <- lowerFunc func
        body' <- lowerTail body k
        let alocType' = lowerType alocType
        return (C.LetFunc (alocType', aloc) func' body')
lowerTail (_, LetGlobalTuple (labelType, label) elements body) k =
        do
        body' <- lowerTail body k
        let labelType' = lowerType labelType
        lowerExprs elements \elementAlocs ->
         do
         let elementPlaces = [ (typ, AAloc element) | (typ, element) <- elementAlocs ]
         return (C.LetGlobalTuple (labelType', label) elementPlaces body')
lowerTail (_, LetGlobalFunc (labelType, label) func body) k =
        do
        func' <- lowerFunc func
        body' <- lowerTail body k
        let labelType' = lowerType labelType
        return (C.LetGlobalFunc (labelType', label) func' body')
lowerTail (_, LetGlobalFuncs tLabels funcs body) k =
        do
        let tLabels' = [ (lowerType typ, label) | (typ, label) <- tLabels ]
        C.LetGlobalFuncs tLabels' <$> mapM lowerFunc funcs
                                  <*> lowerTail body k
lowerTail (_, If (RelOp op l r) c a) k =
        do
        c' <- lowerTail c k
        a' <- lowerTail a k
        lowerExprs [l, r] (\[lAloc, rAloc] -> return (C.If (C.RelOp op lAloc rAloc) c' a'))

lowerExpr :: TExpr -> (C.TAloc -> Env C.Expr) -> Env C.Expr
lowerExpr (_, Value value) k = lowerValue value k
lowerExpr (typ, NumOp op l r) k =
        lowerExprs [l, r] \[lAloc, rAloc] ->
        do
        resultAloc <- lift (genAloc "numResult")
        let typ' = lowerType typ
        C.Let (typ', resultAloc) (typ', C.NumOp op lAloc rAloc) <$> k (typ', resultAloc)
lowerExpr (_, Apply f arg) k =
        lowerExprs [f, arg] \[(fAlocType, fAloc), (argAlocType, argAloc)] ->
        do
        contAloc <- lift (genAloc "retCont")
        resultAloc <- lift (genAloc "funcResult")
        let contPlace = AAloc contAloc
            fPlace = AAloc fAloc
            argPlace = AAloc argAloc
        contBody <- k resultAloc
        return (C.LetCont contAloc (C.Cont resultAloc contBody) (C.CallFunc fPlace contPlace argPlace))
lowerExpr (_, TupleRef tuple offset) k =
        lowerExpr tuple \tupleAloc ->
        do
        resultAloc <- lift (genAloc "tupleRefResult")
        let tuplePlace = AAloc tupleAloc
        C.Let resultAloc (C.TupleRef tuplePlace offset) <$> k resultAloc
lowerExpr (_, Let (_, aloc) val body) k =
        lowerExpr val \valAloc ->
        do
        putAloc aloc valAloc
        lowerExpr body k
lowerExpr (_, LetFunc (_, aloc) func body) k = C.LetFunc aloc <$> lowerFunc func
                                                              <*> lowerExpr body k
lowerExpr (_, LetGlobalTuple (_, label) elements body) k =
        lowerExprs elements \elementAlocs ->
        do
        let elementPlaces = map AAloc elementAlocs
        C.LetGlobalTuple label elementPlaces <$> lowerExpr body k
lowerExpr (_, LetGlobalFunc (_, label) func body) k = C.LetGlobalFunc label <$> lowerFunc func
                                                                            <*> lowerExpr body k
lowerExpr (_, LetGlobalFuncs tLabels funcs body) k =
        do
        let labels = [ label | (_, label) <- tLabels ]
        C.LetGlobalFuncs labels <$> mapM lowerFunc funcs
                                <*> lowerExpr body k
lowerExpr (_, If (RelOp op l r) c a) k =
        lowerExprs [l, r] \[lAloc, rAloc] ->
        do
        joinAloc <- lift (genAloc "joinCont")
        resultAloc <- lift (genAloc "ifResult")
        let joinPlace = AAloc joinAloc
        c' <- lowerTail c joinPlace
        a' <- lowerTail a joinPlace
        joinBody <- k resultAloc
        return (C.LetCont joinAloc (C.Cont resultAloc joinBody)
                          (C.If (C.RelOp op lAloc rAloc) c' a'))

lowerExprs :: [TExpr] -> ([C.TAloc] -> Env C.Expr) -> Env C.Expr
lowerExprs = lowerExprs' []

lowerExprs' :: [C.TAloc] -> [TExpr] -> ([C.TAloc] -> Env C.Expr) -> Env C.Expr
lowerExprs' acc [] k = k (reverse acc)
lowerExprs' acc (expr : rest) k = lowerExpr expr (\exprPlace -> lowerExprs' (exprPlace : acc) rest k)

lowerValue :: TValue -> (C.TAloc -> Env C.Expr) -> Env C.Expr
lowerValue (_, Int i) k =
        do
        tmpAloc <- lift (genAloc "int")
        C.Let tmpAloc (C.Int i) <$> (k tmpAloc)
lowerValue (_, Bool b) k =
        do
        tmpAloc <- lift (genAloc "bool")
        C.Let tmpAloc (C.Bool b) <$> (k tmpAloc)
lowerValue (_, Place (AAloc aloc)) k =
        do
        aloc' <- lowerAloc aloc
        k aloc'
lowerValue (_, Place (ALabel label)) k =
        do
        tmpAloc <- lift (genAloc "label")
        C.Let tmpAloc (C.VLabel label) <$> k tmpAloc

lowerFunc :: Func -> Env C.Func
lowerFunc (Func (_, arg) body) =
        do
        contAloc <- lift (genAloc "fCont")
        let contPlace = AAloc contAloc
        body' <- lowerTail body contPlace
        return (C.Func contAloc arg body')

lowerAloc :: Aloc -> Env Aloc
lowerAloc aloc =
        do
        env <- get
        case M.lookup aloc env of
             Nothing -> return aloc
             Just aloc' -> return aloc'

lowerPlace :: APlace -> Env APlace
lowerPlace (AAloc aloc) = AAloc <$> lowerAloc aloc
lowerPlace p@(ALabel _) = return p

lowerType :: Type -> C.Type
lowerType TInt = C.TInt
lowerType TBool = C.TBool
lowerType (TFunc from to) = C.TFunc (C.TCont (lowerType to)) (lowerType from)
lowerType (TTuple elements) = C.TTuple (map lowerType elements)

putAloc :: Aloc -> Aloc -> Env ()
putAloc aloc aloc' =
        do
        env <- get
        let env' = M.insert aloc aloc' env
        put env'
