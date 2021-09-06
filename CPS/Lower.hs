module CPS.Lower
( lower
)
where

import Compiler.Gensym
import Compiler.Types
import CPS.Types
import qualified CPSClosures.Types as C

import Control.Monad.Writer
import qualified Data.Map as M
import qualified Data.Set as S

type Closures = WriterT [C.Def] (Gensym)

runClosures :: Closures a -> Gensym (a, [C.Def])
runClosures computation = runWriterT computation

lower :: Program -> Gensym C.Program
lower (Program expr) =
        do
        (body, defs) <- runClosures (lowerExpr expr)
        return (C.Program defs (C.Body body))

lowerExpr :: Expr -> Closures C.Expr
lowerExpr (CallFunc f cont arg) =
        do
        codePtrAloc <- lift (genAloc "funcCodePtr")
        let codePtrPlace = AAloc codePtrAloc
        return (C.Let codePtrAloc (C.TupleRef f 0)
                      (C.CallFunc codePtrPlace f cont arg))
lowerExpr (CallCont cont arg) =
        do
        codePtrAloc <- lift (genAloc "contCodePtr")
        let codePtrPlace = AAloc codePtrAloc
        return (C.Let codePtrAloc (C.TupleRef cont 0)
                      (C.CallCont codePtrPlace cont arg))
lowerExpr (Let aloc val body) = C.Let aloc (lowerValue val) <$> lowerExpr body
lowerExpr (LetCont aloc cont body) =
        do
        closureElements <- lowerCont cont
        C.Let aloc (C.Tuple closureElements) <$> lowerExpr body
lowerExpr (LetFunc aloc func body) =
        do
        closureElements <- lowerFunc func
        C.Let aloc (C.Tuple closureElements) <$> lowerExpr body
lowerExpr (LetGlobalTuple label elements body) = C.LetGlobalTuple label elements <$> lowerExpr body
lowerExpr (LetGlobalFunc label func body) =
        do
        closureElements <- lowerFunc func
        C.LetGlobalTuple label closureElements <$> lowerExpr body
lowerExpr (LetGlobalFuncs labels funcs body) =
        do
        closureElements <- mapM lowerFunc funcs
        body' <- lowerExpr body
        return (foldr (\(label, elements) expr -> C.LetGlobalTuple label elements expr)
                      body'
                      (zip labels closureElements))
lowerExpr (If (RelOp op l r) c a) = C.If (C.RelOp op l r) <$> lowerExpr c
                                                          <*> lowerExpr a

lowerValue :: Value -> C.Value
lowerValue (Int i) = C.Int i
lowerValue (Bool b) = C.Bool b
lowerValue (VLabel label) = C.VLabel label
lowerValue (TupleRef tuple offset) = C.TupleRef tuple offset
lowerValue (NumOp op l r) = C.NumOp op l r

lowerFunc :: Func -> Closures [APlace]
lowerFunc (Func cont arg body) =
        do
        body' <- lowerExpr body
        let bodyFree = freeVars body' [cont, arg]
        newFreeAlocs <- mapM regenAloc bodyFree
        let body'' = replace (zip bodyFree newFreeAlocs) body'
            enumeratedNewFreeAlocs = zip [1..] newFreeAlocs
        envAloc <- lift (genAloc "env")
        let envPlace = AAloc envAloc
            body''' = foldr (\(i, freeVar) expr -> C.Let freeVar (C.TupleRef envPlace i) expr)
                            body''
                            enumeratedNewFreeAlocs
        funcLabel <- lift (genLabel "func")
        tell [ C.Func funcLabel envAloc cont arg (C.Body body''') ]
        let funcPlace = ALabel funcLabel
            bodyFreePlaces = map AAloc bodyFree
        return (funcPlace : bodyFreePlaces)

lowerCont :: Cont -> Closures [APlace]
lowerCont (Cont arg body) =
        do
        body' <- lowerExpr body
        let bodyFree = freeVars body' [arg]
        newFreeAlocs <- mapM regenAloc bodyFree
        let body'' = replace (zip bodyFree newFreeAlocs) body'
            enumeratedNewFreeAlocs = zip [1..] newFreeAlocs
        envAloc <- lift (genAloc "env")
        let envPlace = AAloc envAloc
            body''' = foldr (\(i, freeVar) expr -> C.Let freeVar (C.TupleRef envPlace i) expr)
                            body''
                            enumeratedNewFreeAlocs
        contLabel <- lift (genLabel "cont")
        tell [ C.Cont contLabel envAloc arg (C.Body body''') ]
        let contPlace = ALabel contLabel
            bodyFreePlaces = map AAloc bodyFree
        return (contPlace : bodyFreePlaces)

freeVars :: C.Expr -> [Aloc] -> [Aloc]
freeVars expr bound =
        let freeInExpr = freeVarsExpr expr
            boundSet = S.fromList bound
            freeVars = S.difference freeInExpr boundSet
            freeVarsList = S.toList freeVars
        in freeVarsList

freeVarsExpr :: C.Expr -> S.Set Aloc
freeVarsExpr (C.CallFunc func env cont arg) = S.fromList [ aloc | AAloc aloc <- [func, env, cont, arg] ]
freeVarsExpr (C.CallCont cont env arg) = S.fromList [ aloc | AAloc aloc <- [cont, env, arg] ]
freeVarsExpr (C.Let aloc val body) = S.union (freeVarsValue val)
                                             (S.delete aloc (freeVarsExpr body))
freeVarsExpr (C.LetGlobalTuple _ elements body) =
        let freeElements = S.fromList [ aloc | AAloc aloc <- elements ]
            freeInBody = freeVarsExpr body
            freeVars = S.union freeElements freeInBody
        in freeVars
freeVarsExpr (C.If (C.RelOp op l r) c a) =
        let freePred = S.fromList [l, r]
            freeC = freeVarsExpr c
            freeA = freeVarsExpr a
            freeVars = S.unions [freePred, freeC, freeA]
        in freeVars

freeVarsValue :: C.Value -> S.Set Aloc
freeVarsValue (C.Int _) = S.empty
freeVarsValue (C.Bool _) = S.empty
freeVarsValue (C.VLabel _) = S.empty
freeVarsValue (C.Tuple elements) = S.fromList [ aloc | AAloc aloc <- elements ]
freeVarsValue (C.TupleRef tuple _) = S.fromList [ aloc | AAloc aloc <- [tuple] ]
freeVarsValue (C.NumOp _ l r) = S.fromList [l, r]

replace :: [(Aloc, Aloc)] -> C.Expr -> C.Expr
replace replacements expr = replaceExpr (M.fromList replacements) expr

replaceExpr :: M.Map Aloc Aloc -> C.Expr -> C.Expr
replaceExpr env (C.CallFunc f fEnv cont arg) =
        let [f', fEnv', cont', arg'] = replacePlaces env [f, fEnv, cont, arg]
        in C.CallFunc f' fEnv' cont' arg'
replaceExpr env (C.CallCont cont cEnv arg) =
        let [cont', cEnv', arg'] = replacePlaces env [cont, cEnv, arg]
        in C.CallCont cont' cEnv' arg'
replaceExpr env (C.Let aloc val body) = C.Let aloc (replaceValue env val) (replaceExpr env body)
replaceExpr env (C.LetGlobalTuple label elements body) =
        let elements' = replacePlaces env elements
        in C.LetGlobalTuple label elements' (replaceExpr env body)
replaceExpr env (C.If (C.RelOp op l r) c a) =
        let [l', r'] = replaceAlocs env [l, r]
        in C.If (C.RelOp op l' r') (replaceExpr env c) (replaceExpr env a)

replaceValue :: M.Map Aloc Aloc -> C.Value -> C.Value
replaceValue _ (C.Int i) = C.Int i
replaceValue _ (C.Bool b) = C.Bool b
replaceValue _ (C.VLabel label) = C.VLabel label
replaceValue env (C.Tuple elements) = C.Tuple (replacePlaces env elements)
replaceValue env (C.TupleRef tuple offset) = C.TupleRef (replacePlace env tuple) offset
replaceValue env (C.NumOp op l r) =
        let [l', r'] = replaceAlocs env [l, r]
        in C.NumOp op l' r'

replacePlaces :: M.Map Aloc Aloc -> [APlace] -> [APlace]
replacePlaces env places = map (replacePlace env) places

replacePlace :: M.Map Aloc Aloc -> APlace -> APlace
replacePlace env place@(AAloc aloc) = maybe place AAloc (M.lookup aloc env)
replacePlace _ place@(ALabel _) = place

replaceAlocs :: M.Map Aloc Aloc -> [Aloc] -> [Aloc]
replaceAlocs env places = map (replaceAloc env) places

replaceAloc :: M.Map Aloc Aloc -> Aloc -> Aloc
replaceAloc env aloc = maybe aloc id (M.lookup aloc env)

regenAloc :: Aloc -> Closures Aloc
regenAloc (Aloc template _) = lift (genAloc template)
