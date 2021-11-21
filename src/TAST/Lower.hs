module TAST.Lower
( lower
)
where

import Compiler.Gensym
import Compiler.Types
import qualified Lambda.Types as L

import Control.Monad.State
import qualified Data.Map as M
import TAST.Types

type Env = StateT (M.Map Var Aloc) Gensym

evalEnv :: Env a -> Gensym a
evalEnv computation = evalStateT computation M.empty

lower :: Program -> Gensym L.Program
lower (Program def) = evalEnv (L.Program <$> lowerProgramDef def)

lowerProgramDef :: TProgramDef -> Env L.TExpr
lowerProgramDef (_, mainTyp, Main) =
        do
        mainAloc <- lookupVar (Var "main")
        let mainTyp' = lowerType mainTyp
        return (mainTyp', L.Atom (mainTyp', L.AtAloc (mainTyp', mainAloc)))
lowerProgramDef (_, typ, LetValue val restDefs) =
        do
        valLet <- lowerValue val
        restDefs' <- lowerProgramDef restDefs
        let typ' = lowerType typ
        return (typ', valLet restDefs')
lowerProgramDef (_, typ, LetFuncs funcs restDefs) =
        do
        funcsLetRec <- lowerFuncs funcs
        restDefs' <- lowerProgramDef restDefs
        let typ' = lowerType typ
        return (typ', funcsLetRec restDefs')

lowerValue :: TValue -> Env (L.TExpr -> L.Expr)
lowerValue (_, _, Value var body) =
        do
        body' <- lowerExpr body
        aloc <- genPutVar var
        return (L.Let aloc body')

lowerFuncs :: [TFunc] -> Env (L.TExpr -> L.Expr)
lowerFuncs funcs =
        do
        let vars = [ var | (_, _, Func var _ _) <- funcs]
        alocs <- mapM genPutVar vars
        funcBodies <- mapM lowerFunc funcs
        return (L.LetRec alocs funcBodies)

lowerFunc :: TFunc -> Env L.TExpr
lowerFunc (_, _, Func _ args body) =
        encapsulate ( do
                      argAlocs <- mapM genPutVar args
                      body' <- lowerExpr body
                      let body'' = foldr (\arg expr ->
                                          (L.TFunc (tagType arg) (tagType expr), L.Lambda arg expr))
                                         body'
                                         argAlocs
                      return body''
                    )

lowerExpr :: TExpr -> Env L.TExpr
lowerExpr (_, typ, Atom atom) =
        do
        atom' <- lowerAtom atom
        let typ' = lowerType typ
        return (typ', L.Atom atom')
lowerExpr (_, typ, BinOp op l r) =
        do
        l' <- lowerExpr l
        r' <- lowerExpr r
        let typ' = lowerType typ
        return (typ', L.BinOp op l' r')
lowerExpr (_, typ, Apply f arg) =
        do
        f' <- lowerExpr f
        arg' <- lowerExpr arg
        let typ' = lowerType typ
        return (typ', L.Apply f' arg')
lowerExpr (_, typ, Lambda var body) =
        encapsulate ( do
                      aloc <- genPutVar var
                      body' <- lowerExpr body
                      let typ' = lowerType typ
                      return (typ', L.Lambda aloc body')
                    )
lowerExpr (_, typ, Let var val body) =
        encapsulate ( do
                      val' <- lowerExpr val
                      aloc <- genPutVar var
                      body' <- lowerExpr body
                      let typ' = lowerType typ
                      return (typ', L.Let aloc val' body')
                    )
lowerExpr (_, typ, If p c a) =
        do
        p' <- lowerExpr p
        c' <- lowerExpr c
        a' <- lowerExpr a
        let typ' = lowerType typ
        return (typ', L.If p' c' a')

lowerAtom :: TAtom -> Env L.TAtom
lowerAtom (_, typ, Int i) =
        do
        let typ' = lowerType typ
        return (typ', L.Int i)
lowerAtom (_, typ, Bool b) =
        do
        let typ' = lowerType typ
        return (typ', L.Bool b)
lowerAtom (_, typ, AtVar (_, varTyp, var)) =
        do
        aloc <- lookupVar var
        let typ' = lowerType typ
            varTyp' = lowerType varTyp
        return (typ', L.AtAloc (varTyp', aloc))

-- Program is guarenteed to not have undefined variables at this point, so we
-- can simply just return an Aloc and not Maybe Aloc
lookupVar :: Var -> Env Aloc
lookupVar var =
        do
        env <- get
        return (env M.! var)

genPutVar :: TVar -> Env L.TAloc
genPutVar (_, typ, Var template) =
        do
        aloc <- lift (genAloc template)
        env <- get
        put (M.insert (Var template) aloc env)
        let typ' = lowerType typ
        return (typ', aloc)

lowerType :: Type -> L.Type
lowerType TInt = L.TInt
lowerType TBool = L.TBool
lowerType (TFunc f t) = L.TFunc (lowerType f) (lowerType t)

varToString :: TVar -> String
varToString (_, _, Var s) = s

encapsulate :: Env a -> Env a
encapsulate computation =
        do
        env <- get
        result <- computation
        put env
        return result

tagType :: L.Tagged a -> L.Type
tagType (typ, _) = typ
