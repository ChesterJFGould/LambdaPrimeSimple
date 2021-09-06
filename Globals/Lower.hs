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

type Env a = StateT (M.Map Aloc Aloc) (Gensym) a

evalEnv :: Env a -> Gensym a
evalEnv computation = evalStateT computation M.empty

lower :: Program -> Gensym C.Program
lower (Program expr) = evalEnv (C.Program <$> lowerTail expr (ALabel HaltLabel))

lowerTail :: Expr -> APlace -> Env C.Expr
lowerTail (Value value) k = lowerValue value (\vAloc -> return (C.CallCont k (AAloc vAloc)))
lowerTail expr@(NumOp _ _ _) k = lowerExpr expr (\resultAloc -> return (C.CallCont k (AAloc resultAloc)))
lowerTail (Apply f arg) k =
        lowerExprs [f, arg] \[fAloc, argAloc] ->
        do
        let fPlace = AAloc fAloc
            argPlace = AAloc argAloc
        return (C.CallFunc fPlace k argPlace)
lowerTail expr@(TupleRef _ _) k = lowerExpr expr (\refAloc -> return (C.CallCont k (AAloc refAloc)))
lowerTail (Let aloc val body) k =
        lowerExpr val \valAloc ->
        do
        putAloc aloc valAloc
        lowerTail body k
lowerTail (LetFunc aloc func body) k = C.LetFunc aloc <$> lowerFunc func
                                                      <*> lowerTail body k
lowerTail (LetGlobalTuple label elements body) k =
        do
        body' <- lowerTail body k
        lowerExprs elements (\elementAlocs -> return (C.LetGlobalTuple label (map AAloc elementAlocs) body'))
lowerTail (LetGlobalFunc label func body) k = C.LetGlobalFunc label <$> lowerFunc func
                                                                    <*> lowerTail body k
lowerTail (LetGlobalFuncs labels funcs body) k = C.LetGlobalFuncs labels <$> mapM lowerFunc funcs
                                                                         <*> lowerTail body k
lowerTail (If (RelOp op l r) c a) k =
        do
        c' <- lowerTail c k
        a' <- lowerTail a k
        lowerExprs [l, r] (\[lAloc, rAloc] -> return (C.If (C.RelOp op lAloc rAloc) c' a'))

lowerExpr :: Expr -> (Aloc -> Env C.Expr) -> Env C.Expr
lowerExpr (Value value) k = lowerValue value k
lowerExpr (NumOp op l r) k =
        lowerExprs [l, r] \[lAloc, rAloc] ->
        do
        resultAloc <- lift (genAloc "numResult")
        C.Let resultAloc (C.NumOp op lAloc rAloc) <$> k resultAloc
lowerExpr (Apply f arg) k =
        lowerExprs [f, arg] \[fAloc, argAloc] ->
        do
        contAloc <- lift (genAloc "retCont")
        resultAloc <- lift (genAloc "funcResult")
        let contPlace = AAloc contAloc
            fPlace = AAloc fAloc
            argPlace = AAloc argAloc
        contBody <- k resultAloc
        return (C.LetCont contAloc (C.Cont resultAloc contBody) (C.CallFunc fPlace contPlace argPlace))
lowerExpr (TupleRef tuple offset) k =
        lowerExpr tuple \tupleAloc ->
        do
        resultAloc <- lift (genAloc "tupleRefResult")
        let tuplePlace = AAloc tupleAloc
        C.Let resultAloc (C.TupleRef tuplePlace offset) <$> k resultAloc
lowerExpr (Let aloc val body) k =
        lowerExpr val \valAloc ->
        do
        putAloc aloc valAloc
        lowerExpr body k
lowerExpr (LetFunc aloc func body) k = C.LetFunc aloc <$> lowerFunc func
                                                      <*> lowerExpr body k
lowerExpr (LetGlobalTuple label elements body) k =
        lowerExprs elements \elementAlocs ->
        do
        let elementPlaces = map AAloc elementAlocs
        C.LetGlobalTuple label elementPlaces <$> lowerExpr body k
lowerExpr (LetGlobalFunc label func body) k = C.LetGlobalFunc label <$> lowerFunc func
                                                                    <*> lowerExpr body k
lowerExpr (LetGlobalFuncs labels funcs body) k = C.LetGlobalFuncs labels <$> mapM lowerFunc funcs
                                                                         <*> lowerExpr body k
lowerExpr (If (RelOp op l r) c a) k =
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

lowerExprs :: [Expr] -> ([Aloc] -> Env C.Expr) -> Env C.Expr
lowerExprs = lowerExprs' []

lowerExprs' :: [Aloc] -> [Expr] -> ([Aloc] -> Env C.Expr) -> Env C.Expr
lowerExprs' acc [] k = k (reverse acc)
lowerExprs' acc (expr : rest) k = lowerExpr expr (\exprPlace -> lowerExprs' (exprPlace : acc) rest k)

lowerValue :: Value -> (Aloc -> Env C.Expr) -> Env C.Expr
lowerValue (Int i) k =
        do
        tmpAloc <- lift (genAloc "int")
        C.Let tmpAloc (C.Int i) <$> (k tmpAloc)
lowerValue (Bool b) k =
        do
        tmpAloc <- lift (genAloc "bool")
        C.Let tmpAloc (C.Bool b) <$> (k tmpAloc)
lowerValue (Place (AAloc aloc)) k =
        do
        aloc' <- lowerAloc aloc
        k aloc'
lowerValue (Place (ALabel label)) k =
        do
        tmpAloc <- lift (genAloc "label")
        C.Let tmpAloc (C.VLabel label) <$> k tmpAloc

lowerFunc :: Func -> Env C.Func
lowerFunc (Func arg body) =
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

putAloc :: Aloc -> Aloc -> Env ()
putAloc aloc aloc' =
        do
        env <- get
        let env' = M.insert aloc aloc' env
        put env'
