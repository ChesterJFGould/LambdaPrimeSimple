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
        lowerExpr val \valTAloc ->
        do
        putAloc aloc valTAloc
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
         let elementTPlaces = [ (typ, AAloc element) | (typ, element) <- elementAlocs ]
         return (C.LetGlobalTuple (labelType', label) elementTPlaces body')
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
        C.Let (typ', resultAloc) (C.NumOp op lAloc rAloc) <$> k (typ', resultAloc)
lowerExpr (typ, Apply f arg) k =
        lowerExprs [f, arg] \[(fAlocType, fAloc), (argAlocType, argAloc)] ->
        do
        let typ' = lowerType typ
        contAloc <- lift (genAloc "retCont")
        let contType = C.TCont typ'
            contTAloc = (contType, contAloc)
        resultAloc <- lift (genAloc "funcResult")
        let resultType = typ'
            resultTAloc = (resultType, resultAloc)
        let contTPlace = (contType, AAloc contAloc)
            fTPlace = (fAlocType, AAloc fAloc)
            argTPlace = (argAlocType, AAloc argAloc)
        contBody <- k resultTAloc
        return (C.LetCont contTAloc (C.Cont resultTAloc contBody) (C.CallFunc fTPlace contTPlace argTPlace))
lowerExpr (_, TupleRef tuple offset) k =
        lowerExpr tuple \(tupleAlocType@(C.TTuple elementTypes), tupleAloc) ->
        do
        resultAloc <- lift (genAloc "tupleRefResult")
        let resultType = elementTypes !! offset
            resultTAloc = (resultType, resultAloc)
        let tupleTPlace = (tupleAlocType, AAloc tupleAloc)
        C.Let resultTAloc (C.TupleRef tupleTPlace offset) <$> k resultTAloc
lowerExpr (_, Let (_, aloc) val body) k =
        lowerExpr val \valTAloc ->
        do
        putAloc aloc valTAloc
        lowerExpr body k
lowerExpr (_, LetFunc (alocType, aloc) func body) k =
        let alocType' = lowerType alocType
            tAloc = (alocType', aloc)
        in C.LetFunc tAloc <$> lowerFunc func
                           <*> lowerExpr body k
lowerExpr (_, LetGlobalTuple (labelType, label) elements body) k =
        lowerExprs elements \elementAlocs ->
        do
        let elementTPlaces = [ (typ, AAloc aloc) | (typ, aloc) <- elementAlocs ]
            tLabel = (lowerType labelType, label)
        C.LetGlobalTuple tLabel elementTPlaces <$> lowerExpr body k
lowerExpr (_, LetGlobalFunc (labelType, label) func body) k =
        do
        let labelType' = lowerType labelType
            tLabel = (labelType', label)
        C.LetGlobalFunc tLabel <$> lowerFunc func
                               <*> lowerExpr body k
lowerExpr (_, LetGlobalFuncs tLabels funcs body) k =
        do
        let tLabels' = [ (lowerType typ, label) | (typ, label) <- tLabels ]
        C.LetGlobalFuncs tLabels' <$> mapM lowerFunc funcs
                                  <*> lowerExpr body k
lowerExpr (typ, If (RelOp op l r) c a) k =
        lowerExprs [l, r] \[lTAloc, rTAloc] ->
        do
        joinAloc <- lift (genAloc "joinCont")
        resultAloc <- lift (genAloc "ifResult")
        let resultType = lowerType typ
            resultTAloc = (resultType, resultAloc)
        let joinPlace = AAloc joinAloc
            joinType = C.TCont resultType
            joinTAloc = (joinType, joinAloc)
            joinTPlace = (joinType, joinPlace)
        c' <- lowerTail c joinTPlace
        a' <- lowerTail a joinTPlace
        joinBody <- k resultTAloc
        return (C.LetCont joinTAloc (C.Cont resultTAloc joinBody)
                          (C.If (C.RelOp op lTAloc rTAloc) c' a'))

lowerExprs :: [TExpr] -> ([C.TAloc] -> Env C.Expr) -> Env C.Expr
lowerExprs = lowerExprs' []

lowerExprs' :: [C.TAloc] -> [TExpr] -> ([C.TAloc] -> Env C.Expr) -> Env C.Expr
lowerExprs' acc [] k = k (reverse acc)
lowerExprs' acc (expr : rest) k = lowerExpr expr (\exprPlace -> lowerExprs' (exprPlace : acc) rest k)

lowerValue :: TValue -> (C.TAloc -> Env C.Expr) -> Env C.Expr
lowerValue (typ, Int i) k =
        do
        tmpAloc <- lift (genAloc "int")
        let typ' = lowerType typ
            tmpTAloc = (typ', tmpAloc)
        C.Let tmpTAloc (C.Int i) <$> (k tmpTAloc)
lowerValue (typ, Bool b) k =
        do
        tmpAloc <- lift (genAloc "bool")
        let typ' = lowerType typ
            tmpTAloc = (typ', tmpAloc)
        C.Let tmpTAloc (C.Bool b) <$> (k tmpTAloc)
lowerValue (typ, Place (AAloc aloc)) k =
        do
        let typ' = lowerType typ
        tAloc' <- lowerAloc typ' aloc
        k tAloc'
lowerValue (typ, Place (ALabel label)) k =
        do
        tmpAloc <- lift (genAloc "label")
        let typ' = lowerType typ
            tmpTAloc = (typ', tmpAloc)
            tLabel = (typ', label)
        C.Let tmpTAloc (C.VLabel tLabel) <$> k tmpTAloc

lowerFunc :: Func -> Env C.Func
lowerFunc (Func (argType, arg) body@(bodyType, _)) =
        do
        contAloc <- lift (genAloc "fCont")
        let bodyType' = lowerType bodyType
            contType = C.TCont bodyType'
            contTAloc = (contType, contAloc)
            contPlace = AAloc contAloc
            contTPlace = (contType, contPlace)
        let argType' = lowerType argType
            tArg = (argType', arg)
        body' <- lowerTail body contTPlace
        return (C.Func contTAloc tArg body')

lowerAloc :: C.Type -> Aloc -> Env C.TAloc
lowerAloc typ aloc =
        do
        env <- get
        case M.lookup aloc env of
             Nothing -> return (typ, aloc)
             Just aloc' -> return aloc'

lowerType :: Type -> C.Type
lowerType TInt = C.TInt
lowerType TBool = C.TBool
lowerType (TFunc from to) = C.TFunc (C.TCont (lowerType to)) (lowerType from)
lowerType (TTuple elements) = C.TTuple (map lowerType elements)

putAloc :: Aloc -> C.TAloc -> Env ()
putAloc aloc tAloc =
        do
        env <- get
        let env' = M.insert aloc tAloc env
        put env'
