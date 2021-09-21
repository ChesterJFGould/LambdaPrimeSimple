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
        let (_, defVarType, defVar) = extractDefVar def
        defLabel <- lowerLabel defVar
        let defLabelType = lowerType defVarType
        putVarLabel defVar defLabel
        restDefs' <- lowerProgramDef restDefs
        let typ' = lowerType typ
        return (typ', L.LetGlobal (defLabelType, defLabel) defBody restDefs')
lowerProgramDef (_, typ, LetRecDefs defs restDefs) =
        do
        let defTVars = map extractDefVar defs
            defVars = [ defVar | (_, _, defVar) <- defTVars ]
            defVarTypes = [ defVarType | (_, defVarType, _) <- defTVars ]
        defLabels <- mapM lowerLabel defVars
        let defLabelTypes = map lowerType defVarTypes
            defTLabels = zip defLabelTypes defLabels
        mapM (uncurry putVarLabel) (zip defVars defLabels)
        defBodies <- mapM lowerDef defs
        restDefs' <- lowerProgramDef restDefs
        let typ' = lowerType typ
        return (typ', L.LetGlobals defTLabels defBodies restDefs')

lowerDef :: Def -> Env L.TExpr
lowerDef (Def _ args body) = encapsulate (lowerDefArgs args body)

lowerDefArgs :: [TVar] -> TBody -> Env L.TExpr
lowerDefArgs [] body = lowerBody body
lowerDefArgs ((_, varType, var) : rest) body =
        do
        aloc <- lowerAloc var
        putVarAloc var aloc
        body'@(bodyType, _) <- lowerDefArgs rest body
        let varType' = lowerType varType
        return (L.TFunc varType' bodyType, L.Lambda (varType', aloc) body')

lowerBody :: TBody -> Env L.TExpr
lowerBody (_, _, Body expr) = lowerExpr expr

lowerExpr :: TExpr -> Env L.TExpr
lowerExpr (_, typ, Value value) =
        do
        value' <- lowerValue value
        return (lowerType typ, L.Value value')
lowerExpr (_, typ, BinOp op l r) =
        do
        l' <- lowerExpr l
        r' <- lowerExpr r
        return (lowerType typ, L.BinOp op l' r')
lowerExpr (_, typ, Apply f arg) =
        do
        f' <- lowerExpr f
        arg' <- lowerExpr arg
        return (lowerType typ, L.Apply f' arg')
lowerExpr (_, typ, Lambda (_, varType, var) body) =
        encapsulate ( do
                      aloc <- lowerAloc var
                      putVarAloc var aloc
                      body' <- lowerExpr body
                      return (lowerType typ, L.Lambda (lowerType varType, aloc) body')
                    )
lowerExpr (_, typ, Let (_, varType, var) val body) =
        encapsulate ( do
                      aloc <- lowerAloc var
                      val' <- lowerExpr val
                      putVarAloc var aloc
                      body' <- lowerExpr body
                      return (lowerType typ, L.Let (lowerType varType, aloc) val' body')
                    )
lowerExpr (_, typ, If p c a) =
        do
        p' <- lowerExpr p
        c' <- lowerExpr c
        a' <- lowerExpr a
        return (lowerType typ, L.If p' c' a')

lowerValue :: TValue -> Env L.TValue
lowerValue (_, typ, Int i) = return (lowerType typ, L.Int i)
lowerValue (_, typ, Bool b) = return (lowerType typ, L.Bool b)
lowerValue (_, typ, VVar (_, _, var)) =
        do
        var' <- lowerVar var
        return (lowerType typ, L.Place var')

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

extractDefVar :: Def -> TVar
extractDefVar (Def var _ _) = var

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
