module AST.Lower
( lower
)
where

import AST.Types
import Compiler.Gensym
import Compiler.Types
import qualified Lambda.Types as L

import Control.Monad.State
import qualified Data.Map as M

type Env = StateT (M.Map Var APlace) (Gensym)

evalEnv :: Env a -> Gensym a
evalEnv computation = evalStateT computation M.empty

lower :: Program -> Gensym L.Program
lower (Program def) = evalEnv (L.Program <$> lowerProgramDef def)

lowerProgramDef :: TProgramDef -> Env L.Expr
lowerProgramDef (_, Main) = (L.Value . L.Place) <$> lowerVar (Var "main")
lowerProgramDef (_, LetDef def restDefs) =
        do
        defBody <- lowerDef def
        let defVar = extractDefVar def
        defLabel <- lowerLabel defVar
        putVarLabel defVar defLabel
        L.LetGlobal defLabel defBody <$> lowerProgramDef restDefs
lowerProgramDef (_, LetRecDefs defs restDefs) =
        do
        let defVars = map extractDefVar defs
        defLabels <- mapM lowerLabel defVars
        mapM (uncurry putVarLabel) (zip defVars defLabels)
        defBodies <- mapM lowerDef defs
        L.LetGlobals defLabels defBodies <$> lowerProgramDef restDefs

lowerDef :: TDef -> Env L.Expr
lowerDef (_, Def _ _ args body) =
        encapsulate ( do
                      let argVars = [ argVar | (_, argVar) <- args ]
                      argAlocs <- mapM lowerAloc argVars
                      mapM (uncurry putVarAloc) (zip argVars argAlocs)
                      body' <- lowerBody body
                      return (foldr L.Lambda body' argAlocs)
                    )

lowerBody :: TBody -> Env L.Expr
lowerBody (_, Body expr) = lowerExpr expr

lowerExpr :: TExpr -> Env L.Expr
lowerExpr (_, Value value) = L.Value <$> lowerValue value
lowerExpr (_, BinOp op l r) = L.BinOp op <$> lowerExpr l
                                         <*> lowerExpr r
lowerExpr (_, Apply f arg) = L.Apply <$> lowerExpr f
                                     <*> lowerExpr arg
lowerExpr (_, Lambda (_, var) _ bodyExpr) =
        encapsulate ( do
                      aloc <- lowerAloc var
                      putVarAloc var aloc
                      L.Lambda aloc <$> lowerExpr bodyExpr
                    )
lowerExpr (_, Let (_, var) val body) =
        encapsulate ( do
                      aloc <- lowerAloc var
                      val' <- lowerExpr val
                      putVarAloc var aloc
                      L.Let aloc val' <$> lowerExpr body
                    )
lowerExpr (_, If p c a) = L.If <$> lowerExpr p
                               <*> lowerExpr c
                               <*> lowerExpr a

lowerValue :: TValue -> Env L.Value
lowerValue (_, Int i) = return (L.Int i)
lowerValue (_, Bool b) = return (L.Bool b)
lowerValue (_, VVar (_, var)) = L.Place <$> lowerVar var

lowerLabel :: Var -> Env Label
lowerLabel (Var template) = lift (genLabel template)

lowerAloc :: Var -> Env Aloc
lowerAloc (Var template) = lift (genAloc template)

lowerVar :: Var -> Env APlace
lowerVar var =
        do
        env <- get
        return (env M.! var)

extractDefVar :: TDef -> Var
extractDefVar (_, Def (_, var) _ _ _) = var

putVarLabel :: Var -> Label -> Env ()
putVarLabel var label =
        do
        env <- get
        let env' = M.insert var (ALabel label) env
        put env'

putVarAloc :: Var -> Aloc -> Env ()
putVarAloc var aloc =
        do
        env <- get
        let env' = M.insert var (AAloc aloc) env
        put env'

encapsulate :: Env a -> Env a
encapsulate computation =
        do
        env <- get
        result <- computation
        put env
        return result
