module TAST.Lower
( lower
)
where

import TAST.Types
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

lowerProgramDef :: TProgramDef -> Env L.TExpr
lowerProgramDef (_, typ, Main) =
        do
        mainVar <- lowerVar (Var "main")
        let typ' = lowerType typ
        return (typ', L.Value (typ', L.Place mainVar))
lowerProgramDef (_, typ, LetDef def restDefs) =
        do
        defBody <- lowerDef def
        let defVar = extractDefVar def
        defLabel <- lowerLabel defVar
        putVarLabel defVar defLabel
        restDefs' <- lowerProgramDef restDefs
        let typ' = lowerType typ
        return (typ', L.LetGlobal defLabel defBody restDefs')
lowerProgramDef (_, typ, LetRecDefs defs restDefs) =
        do
        let defVars = map extractDefVar defs
        defLabels <- mapM lowerLabel defVars
        mapM (uncurry putVarLabel) (zip defVars defLabels)
        defBodies <- mapM lowerDef defs
        restDefs' <- lowerProgramDef restDefs
        let typ' = lowerType typ
        return (typ', L.LetGlobals defLabels defBodies restDefs')

lowerDef :: Def -> Env L.TExpr
lowerDef (Def _ args body) =
        encapsulate ( do
                      let argVars = [ argVar | (_, _, argVar) <- args ]
                      argAlocs <- mapM lowerAloc argVars
                      mapM (uncurry putVarAloc) (zip argVars argAlocs)
                      body' <- lowerBody body
                      return (foldr L.Lambda body' argAlocs)
                    )

lowerBody :: TBody -> Env L.TExpr
lowerBody (_, typ, Body expr) = (lowerType typ, lowerExpr expr)

lowerExpr :: TExpr -> Env L.TExpr
lowerExpr (_, typ, Value value) =
	do

	L.Value <$> lowerValue value
lowerExpr (_, _, BinOp op l r) = L.BinOp op <$> lowerExpr l
                                         <*> lowerExpr r
lowerExpr (_, _, Apply f arg) = L.Apply <$> lowerExpr f
                                     <*> lowerExpr arg
lowerExpr (_, _, Lambda (_, _, var) bodyExpr) =
        encapsulate ( do
                      aloc <- lowerAloc var
                      putVarAloc var aloc
                      L.Lambda aloc <$> lowerExpr bodyExpr
                    )
lowerExpr (_, _, Let (_, _, var) val body) =
        encapsulate ( do
                      aloc <- lowerAloc var
                      val' <- lowerExpr val
                      putVarAloc var aloc
                      L.Let aloc val' <$> lowerExpr body
                    )
lowerExpr (_, _, If p c a) = L.If <$> lowerExpr p
                               <*> lowerExpr c
                               <*> lowerExpr a

lowerValue :: TValue -> Env L.TValue
lowerValue (_, _, Int i) = return (L.Int i)
lowerValue (_, _, Bool b) = return (L.Bool b)
lowerValue (_, _, VVar (_, _, var)) = L.Place <$> lowerVar var

lowerLabel :: Var -> Env Label
lowerLabel (Var template) = lift (genLabel template)

lowerAloc :: Var -> Env Aloc
lowerAloc (Var template) = lift (genAloc template)

lowerVar :: Var -> Env APlace
lowerVar var@(Var varName) =
        do
        env <- get
        return (env M.! var)

lowerType :: Type -> L.Type
lowerType TInt = L.TInt
lowerType TBool = L.TBool
lowerType (TFunc from to) = L.TFunc (lowerType from)
                                    (lowerType to)

extractDefVar :: Def -> Var
extractDefVar (Def (_, _, var) _ _) = var

putVarLabel :: Var -> Label -> Env ()
putVarLabel var@(Var varName) label =
        do
        env <- get
        let env' = M.insert var (ALabel label) env
        put env'

putVarAloc :: Var -> Aloc -> Env ()
putVarAloc var@(Var varName) aloc =
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
