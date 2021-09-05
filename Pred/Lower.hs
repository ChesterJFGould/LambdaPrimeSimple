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

type Env = StateT (M.Map Label G.Expr) (Gensym)

evalEnv :: Env a -> Gensym a
evalEnv computation = evalStateT computation M.empty

lower :: Program -> Gensym G.Program
lower (Program expr) = evalEnv (G.Program <$> lowerExpr expr)

lowerExpr :: Expr -> Env G.Expr
lowerExpr (Value value) = lowerValue value
lowerExpr (NumOp op l r) = G.NumOp op <$> lowerExpr l
                                      <*> lowerExpr r
lowerExpr (Apply f arg) = G.Apply <$> lowerExpr f
                                  <*> lowerExpr arg
lowerExpr (Lambda arg body) =
        do
        lambdaAloc <- lift (genAloc "lambda")
        body' <- lowerExpr body
        let lambdaExpr = G.Value (G.Place (AAloc lambdaAloc))
        return (G.LetFunc lambdaAloc (G.Func arg body') lambdaExpr)
lowerExpr (Let aloc val body) = G.Let aloc <$> lowerExpr val
                                           <*> lowerExpr body
lowerExpr (LetGlobal label (Lambda arg lambdaBody) body) =
        do
        lambdaBody' <- lowerExpr lambdaBody
        G.LetGlobalFunc label (G.Func arg lambdaBody') <$> lowerExpr body
lowerExpr (LetGlobal label val body) =
        do
        let derefLabel = G.TupleRef (G.Value (G.Place (ALabel label))) 0
        putLabelExpr label derefLabel
        val' <- lowerExpr val
        G.LetGlobalTuple label [val'] <$> lowerExpr body
lowerExpr (LetGlobals labels bodies body) =
        do
        let proveLambda (Lambda aloc body) = (aloc, body)
            proveLambda _ = error "cannot lower non-immediate function in letrec"
            argBodies = map proveLambda bodies
            (args, bodies') = unzip argBodies
        bodies'' <- mapM lowerExpr bodies'
        let funcs = zipWith G.Func args bodies''
        G.LetGlobalFuncs labels funcs <$> lowerExpr body
lowerExpr (If p c a) = G.If <$> lowerPred p
                            <*> lowerExpr c
                            <*> lowerExpr a

lowerPred :: Pred -> Env G.Pred
lowerPred (RelOp op l r) = G.RelOp op <$> lowerExpr l
                                      <*> lowerExpr r

lowerValue :: Value -> Env G.Expr
lowerValue (Int i) = return (G.Value (G.Int i))
lowerValue (Bool b) = return (G.Value (G.Bool b))
lowerValue (Place p@(AAloc _)) = return (G.Value (G.Place p))
lowerValue (Place (ALabel label)) = lowerLabel label

lowerLabel :: Label -> Env G.Expr
lowerLabel label =
        do
        env <- get
        let maybeExpr = M.lookup label env
        case maybeExpr of
             Nothing -> return (G.Value (G.Place (ALabel label)))
             Just expr -> return expr

encapsulate :: Env a -> Env a
encapsulate computation =
        do
        env <- get
        result <- computation
        put env
        return result

putLabelExpr :: Label -> G.Expr -> Env ()
putLabelExpr label expr =
        do
        env <- get
        let env' = M.insert label expr env
        put env'
