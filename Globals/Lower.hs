{-# LANGUAGE BlockArguments #-}
module Globals.Lower
( lower
)
where

import Compiler.Gensym
import Compiler.Types
import qualified CPS.Types as C
import Globals.Types

lower :: Program -> Gensym C.Program
lower (Program expr) = C.Program <$> lowerTail expr (ALabel HaltLabel)

lowerTail :: Expr -> APlace -> Gensym C.Expr
lowerTail (Value value) k = lowerValue value (\vPlace -> return (C.CallCont k vPlace))
lowerTail expr@(NumOp _ _ _) k = lowerExpr expr (\resultPlace -> return (C.CallCont k resultPlace))
lowerTail (Apply f arg) k =
        lowerExprs [f, arg] \[fPlace, argPlace] ->
        return (C.CallFunc fPlace k argPlace)
lowerTail expr@(TupleRef _ _) k = lowerExpr expr (\refPlace -> return (C.CallCont k refPlace))
lowerTail (Let aloc val body) k =
        do
        letContAloc <- genAloc "letCont"
        let letContPlace = AAloc letContAloc
        val' <- lowerTail val letContPlace
        body' <- lowerTail body k
        return (C.LetCont letContAloc (C.Cont aloc body') val')
lowerTail (LetFunc aloc func body) k = C.LetFunc aloc <$> lowerFunc func
                                                      <*> lowerTail body k
lowerTail (LetGlobalTuple label elements body) k =
        do
        body' <- lowerTail body k
        lowerExprs elements (\elementPlaces -> return (C.LetGlobalTuple label elementPlaces body'))
lowerTail (LetGlobalFunc label func body) k = C.LetGlobalFunc label <$> lowerFunc func
                                                                    <*> lowerTail body k
lowerTail (LetGlobalFuncs labels funcs body) k = C.LetGlobalFuncs labels <$> mapM lowerFunc funcs
                                                                         <*> lowerTail body k
lowerTail (If (RelOp op l r) c a) k =
        do
        c' <- lowerTail c k
        a' <- lowerTail a k
        lowerExprs [l, r] (\[AAloc lAloc, AAloc rAloc] -> return (C.If (C.RelOp op lAloc rAloc) c' a'))

lowerExpr :: Expr -> (APlace -> Gensym C.Expr) -> Gensym C.Expr
lowerExpr (Value value) k = lowerValue value k
lowerExpr (NumOp op l r) k =
        lowerExprs [l, r] \[AAloc lAloc, AAloc rAloc] ->
        do
        resultAloc <- genAloc "numResult"
        let resultPlace = AAloc resultAloc
        C.Let resultAloc (C.NumOp op lAloc rAloc) <$> k resultPlace
lowerExpr (Apply f arg) k =
        lowerExprs [f, arg] \[fPlace, argPlace] ->
        do
        contAloc <- genAloc "retCont"
        resultAloc <- genAloc "funcResult"
        let contPlace = AAloc contAloc
            resultPlace = AAloc resultAloc
        contBody <- k resultPlace
        return (C.LetCont contAloc (C.Cont resultAloc contBody) (C.CallFunc fPlace contPlace argPlace))
lowerExpr (TupleRef tuple offset) k =
        lowerExpr tuple \tuplePlace ->
        do
        resultAloc <- genAloc "tupleRefResult"
        let resultPlace = AAloc resultAloc
        C.Let resultAloc (C.TupleRef tuplePlace offset) <$> k resultPlace
lowerExpr (Let aloc val body) k =
        do
        contAloc <- genAloc "letCont"
        let contPlace = AAloc contAloc
        val' <- lowerTail val contPlace
        body' <- lowerExpr body k
        return (C.LetCont contAloc (C.Cont aloc body') val')
lowerExpr (LetFunc aloc func body) k = C.LetFunc aloc <$> lowerFunc func
                                                      <*> lowerExpr body k
lowerExpr (LetGlobalTuple label elements body) k =
        lowerExprs elements \elementPlaces ->
        C.LetGlobalTuple label elementPlaces <$> lowerExpr body k
lowerExpr (LetGlobalFunc label func body) k = C.LetGlobalFunc label <$> lowerFunc func
                                                                    <*> lowerExpr body k
lowerExpr (LetGlobalFuncs labels funcs body) k = C.LetGlobalFuncs labels <$> mapM lowerFunc funcs
                                                                         <*> lowerExpr body k
lowerExpr (If (RelOp op l r) c a) k =
        lowerExprs [l, r] \[AAloc lAloc, AAloc rAloc] ->
        do
        joinAloc <- genAloc "joinCont"
        resultAloc <- genAloc "ifResult"
        let joinPlace = AAloc joinAloc
            resultPlace = AAloc resultAloc
        c' <- lowerTail c joinPlace
        a' <- lowerTail a joinPlace
        joinBody <- k resultPlace
        return (C.LetCont joinAloc (C.Cont resultAloc joinBody)
                          (C.If (C.RelOp op lAloc rAloc) c' a'))

lowerExprs :: [Expr] -> ([APlace] -> Gensym C.Expr) -> Gensym C.Expr
lowerExprs = lowerExprs' []

lowerExprs' :: [APlace] -> [Expr] -> ([APlace] -> Gensym C.Expr) -> Gensym C.Expr
lowerExprs' acc [] k = k (reverse acc)
lowerExprs' acc (expr : rest) k = lowerExpr expr (\exprPlace -> lowerExprs' (exprPlace : acc) rest k)

lowerValue :: Value -> (APlace -> Gensym C.Expr) -> Gensym C.Expr
lowerValue (Int i) k =
        do
        tmpAloc <- genAloc "int"
        let tmpPlace = AAloc tmpAloc
        C.Let tmpAloc (C.Int i) <$> (k tmpPlace)
lowerValue (Bool b) k =
        do
        tmpAloc <- genAloc "bool"
        let tmpPlace = AAloc tmpAloc
        C.Let tmpAloc (C.Bool b) <$> (k tmpPlace)
lowerValue (Place p) k = k p

lowerFunc :: Func -> Gensym C.Func
lowerFunc (Func arg body) =
        do
        contAloc <- genAloc "fCont"
        let contPlace = AAloc contAloc
        body' <- lowerTail body contPlace
        return (C.Func contAloc arg body')
