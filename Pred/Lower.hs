module Pred.Lower
( lower
)
where

import Compiler.Gensym
import Compiler.Types
import qualified Globals.Types as G
import Pred.Types

import Control.Monad.State
import qualified Data.Map as M

type Env = StateT (M.Map Label G.TExpr) (Gensym)

evalEnv :: Env a -> Gensym a
evalEnv computation = evalStateT computation M.empty

lower :: Program -> Gensym G.Program
lower (Program expr) = evalEnv (G.Program <$> lowerExpr expr)

lowerExpr :: TExpr -> Env G.TExpr
lowerExpr (typ, Value value) =
        do
        value' <- lowerValue value
        return value'
lowerExpr (typ, NumOp op l r) =
        do
        l' <- lowerExpr l
        r' <- lowerExpr r
        return (lowerType typ, G.NumOp op l' r')
lowerExpr (typ, Apply f arg) =
        do
        f' <- lowerExpr f
        arg' <- lowerExpr arg
        return (lowerType typ, G.Apply f' arg')
lowerExpr (typ, Lambda (argType, arg) body@(bodyType, _)) =
        do
        lambdaAloc <- lift (genAloc "lambda")
        body' <- lowerExpr body
        let typ' = lowerType typ
            argType' = lowerType argType
            bodyType' = lowerType bodyType
            lambdaExpr = (typ', G.Value (typ', G.Place (AAloc lambdaAloc)))
        return (typ', G.LetFunc ((G.TFunc argType' bodyType'), lambdaAloc) (G.Func (argType', arg) body') lambdaExpr)
lowerExpr (typ, Let (alocType, aloc) val body) =
        do
        val' <- lowerExpr val
        body' <- lowerExpr body
        let typ' = lowerType typ
            alocType' = lowerType alocType
        return (typ', G.Let (alocType', aloc) val' body')
lowerExpr (typ, LetGlobal (labelType, label) (fType, Lambda (argType, arg) lambdaBody) body) =
        do
        lambdaBody' <- lowerExpr lambdaBody
        body' <- lowerExpr body
        let typ' = lowerType typ
            labelType' = lowerType labelType
            fType' = lowerType fType
            argType' = lowerType argType
        return (typ', G.LetGlobalFunc (labelType', label) (G.Func (argType', arg) lambdaBody') body')
lowerExpr (typ, LetGlobal (labelType, label) val body@(bodyType, _)) =
        do
        let labelType' = lowerType labelType
            tupleType = G.TTuple [labelType']
            derefLabel = (labelType', G.TupleRef (tupleType, G.Value (tupleType, G.Place (ALabel label))) 0)
        putLabelExpr label derefLabel
        val' <- lowerExpr val
        body' <- lowerExpr body
        let bodyType' = lowerType bodyType
        return (bodyType', G.LetGlobalTuple (tupleType, label) [val'] body')
lowerExpr (typ, LetGlobals tLabels bodies body) =
        do
        let proveLambda (_, Lambda (alocType, aloc) body) = ((lowerType alocType, aloc), body)
            proveLambda _ = error "cannot lower non-immediate function in letrec"
            argBodies = map proveLambda bodies
            (args, bodies') = unzip argBodies
        bodies'' <- mapM lowerExpr bodies'
        let funcs = zipWith G.Func args bodies''
            tLabels' = [ (lowerType labelType, label) | (labelType, label) <- tLabels ]
        body' <- lowerExpr body
        return (lowerType typ, G.LetGlobalFuncs tLabels' funcs body')
lowerExpr (typ, If p c a) =
        do
        p' <- lowerPred p
        c' <- lowerExpr c
        a' <- lowerExpr a
        let typ' = lowerType typ
        return (typ', G.If p' c' a')

lowerPred :: Pred -> Env G.Pred
lowerPred (RelOp op l r) = G.RelOp op <$> lowerExpr l
                                         <*> lowerExpr r

lowerValue :: TValue -> Env G.TExpr
lowerValue (typ, Int i) =
        do
        let typ' = lowerType typ
        return (typ', G.Value (typ', G.Int i))
lowerValue (typ, Bool b) =
        do
        let typ' = lowerType typ
        return (typ', G.Value (typ', G.Bool b))
lowerValue (typ, Place p@(AAloc _)) =
        do
        let typ' = lowerType typ
        return (typ', G.Value (typ', G.Place p))
lowerValue (typ, Place (ALabel label)) =
        do
        label' <- lowerLabel typ label
        return label'

lowerLabel :: Type -> Label -> Env G.TExpr
lowerLabel labelType label =
        do
        env <- get
        let maybeExpr = M.lookup label env
        case maybeExpr of
             Nothing -> let labelType' = lowerType labelType
                        in  return (labelType', G.Value (labelType', G.Place (ALabel label)))
             Just expr -> return expr

lowerType :: Type -> G.Type
lowerType TInt = G.TInt
lowerType TBool = G.TBool
lowerType (TFunc from to) = G.TFunc (lowerType from) (lowerType to)

encapsulate :: Env a -> Env a
encapsulate computation =
        do
        env <- get
        result <- computation
        put env
        return result

putLabelExpr :: Label -> G.TExpr -> Env ()
putLabelExpr label expr =
        do
        env <- get
        let env' = M.insert label expr env
        put env'
