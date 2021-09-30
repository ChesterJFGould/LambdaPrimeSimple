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
lowerExpr (CallFunc (fType, f) (contType, cont) (argType, arg)) =
        do
        let fType' = lowerType fType
            tF = (fType', f)
            contType' = lowerType contType
            tCont = (contType', cont)
            argType' = lowerType argType
            tArg = (argType', arg)
        codePtrAloc <- lift (genAloc "funcCodePtr")
        let codePtrType = case fType' of
                               C.TClosure typ -> typ
                               _ -> error "called a non-func"
            codePtrTAloc = (codePtrType, codePtrAloc)
            codePtrPlace = AAloc codePtrAloc
            codePtrTPlace = (codePtrType, codePtrPlace)
        return (C.Let codePtrTAloc (C.TupleRef tF 0)
                      (C.CallFunc codePtrTPlace tF tCont tArg))
lowerExpr (CallCont (contType, cont) (argType, arg)) =
        do
        let contType' = lowerType contType
            tCont = (contType', cont)
            argType' = lowerType argType
            tArg = (argType', arg)
        codePtrAloc <- lift (genAloc "contCodePtr")
        let codePtrType = case contType' of
                               C.TClosure typ -> typ
                               _ -> error "called a non-cont"
            codePtrTAloc = (codePtrType, codePtrAloc)
            codePtrPlace = AAloc codePtrAloc
            codePtrTPlace = (codePtrType, codePtrPlace)
        return (C.Let codePtrTAloc (C.TupleRef tCont 0)
                      (C.CallCont codePtrTPlace tCont tArg))
lowerExpr (Let (alocType, aloc) val body) =
        do
        let alocType' = lowerType alocType
            tAloc = (alocType', aloc)
        C.Let tAloc (lowerValue val) <$> lowerExpr body
lowerExpr (LetCont (alocType, aloc) cont body) =
        do
        let alocType' = lowerType alocType
            tAloc = (alocType', aloc)
        closureElements <- lowerCont cont
        C.Let tAloc (C.Tuple closureElements) <$> lowerExpr body
lowerExpr (LetFunc (alocType, aloc) func body) =
        do
        let alocType' = lowerType alocType
            tAloc = (alocType', aloc)
        closureElements <- lowerFunc func
        C.Let tAloc (C.Tuple closureElements) <$> lowerExpr body
lowerExpr (LetGlobalTuple (labelType, label) tElements body) =
        do
        let tElements' = [ (lowerType typ, element) | (typ, element) <- tElements ]
            labelType' = lowerType labelType
            tLabel = (labelType', label)
        C.LetGlobalTuple tLabel tElements' <$> lowerExpr body
lowerExpr (LetGlobalFunc (labelType, label) func body) =
        do
        let labelType' = lowerType labelType
            tLabel = (labelType', label)
        closureElements <- lowerFunc func
        C.LetGlobalTuple tLabel closureElements <$> lowerExpr body
lowerExpr (LetGlobalFuncs tLabels funcs body) =
        do
        closureElements <- mapM lowerFunc funcs
        body' <- lowerExpr body
        let labels = [ (lowerType typ, label) | (typ, label) <- tLabels ]
        return (foldr (\(label, elements) expr -> C.LetGlobalTuple label elements expr)
                      body'
                      (zip labels closureElements))
lowerExpr (If (RelOp op (lType, l) (rType, r)) c a) =
        do
        let lType' = lowerType lType
            tL = (lType', l)
            rType' = lowerType rType
            tR = (rType', r)
        C.If (C.RelOp op tL tR) <$> lowerExpr c
                                                                    <*> lowerExpr a

lowerValue :: Value -> C.Value
lowerValue (Int i) = C.Int i
lowerValue (Bool b) = C.Bool b
lowerValue (VLabel (labelType, label)) = C.VLabel (lowerType labelType, label)
lowerValue (TupleRef (tupleType, tuple) offset) = C.TupleRef (lowerType tupleType, tuple) offset
lowerValue (NumOp op (lType, l) (rType, r)) = C.NumOp op (lowerType lType, l) (lowerType rType, r)

lowerFunc :: Func -> Closures [C.TAPlace]
lowerFunc (Func (contType, cont) (argType, arg) body) =
        do
        body' <- lowerExpr body
        let contType' = lowerType contType
            tCont = (contType', cont)
            argType' = lowerType argType
            tArg = (argType', arg)
            bodyFreeTyped = freeVars body' [tCont, tArg]
            (bodyFreeTypes, bodyFree) = unzip bodyFreeTyped
        newFreeAlocs <- mapM regenAloc bodyFree
        let body'' = replace (zip bodyFree newFreeAlocs) body'
            newFreeTAlocs = zip bodyFreeTypes newFreeAlocs
            enumeratedNewFreeTAlocs = zip [1..] newFreeTAlocs
        envAloc <- lift (genAloc "env")
        let funcType = C.TFunc contType' argType'
            envType = C.TTuple (funcType : bodyFreeTypes)
            envPlace = AAloc envAloc
            envTPlace = (envType, envPlace)
            body''' = foldr (\(i, freeVar) expr -> C.Let freeVar (C.TupleRef envTPlace i) expr)
                            body''
                            enumeratedNewFreeTAlocs
        funcLabel <- lift (genLabel "func")
        let funcTLabel = (funcType, funcLabel)
            envTAloc = (envType, envAloc)
        tell [ C.Func funcTLabel envTAloc tCont tArg (C.Body body''') ]
        let funcPlace = ALabel funcLabel
            funcTPlace = (funcType, funcPlace)
            bodyFreePlaces = map AAloc bodyFree
            bodyFreeTPlaces = zip bodyFreeTypes bodyFreePlaces
        return (funcTPlace : bodyFreeTPlaces)

lowerCont :: Cont -> Closures [C.TAPlace]
lowerCont (Cont (argType, arg) body) =
        do
        body' <- lowerExpr body
        let argType' = lowerType argType
            tArg = (argType', arg)
            bodyFreeTyped = freeVars body' [tArg]
            (bodyFreeTypes, bodyFree) = unzip bodyFreeTyped
        newFreeAlocs <- mapM regenAloc bodyFree
        let body'' = replace (zip bodyFree newFreeAlocs) body'
            newFreeTAlocs = zip bodyFreeTypes newFreeAlocs
            enumeratedNewFreeTAlocs = zip [1..] newFreeTAlocs
        envAloc <- lift (genAloc "env")
        let contType = C.TCont argType'
            envType = C.TTuple (contType : bodyFreeTypes)
            envPlace = AAloc envAloc
            envTPlace = (envType, envPlace)
            body''' = foldr (\(i, freeVar) expr -> C.Let freeVar (C.TupleRef envTPlace i) expr)
                            body''
                            enumeratedNewFreeTAlocs
        contLabel <- lift (genLabel "cont")
        let contTLabel = (contType, contLabel)
            envTAloc = (envType, envAloc)
        tell [ C.Cont contTLabel envTAloc tArg (C.Body body''') ]
        let contPlace = ALabel contLabel
            contTPlace = (contType, contPlace)
            bodyFreePlaces = map AAloc bodyFree
            bodyFreeTPlaces = zip bodyFreeTypes bodyFreePlaces
        return (contTPlace : bodyFreeTPlaces)

lowerType :: Type -> C.Type
lowerType TInt = C.TInt
lowerType TBool = C.TBool
lowerType (TFunc cont arg) = C.TClosure (C.TFunc (lowerType cont) (lowerType arg))
lowerType (TCont arg) = C.TClosure (C.TCont (lowerType arg))
lowerType (TTuple elements) = C.TTuple (map lowerType elements)

freeVars :: C.Expr -> [C.TAloc] -> [C.TAloc]
freeVars expr bound =
        let freeInExpr = freeVarsExpr expr
            boundSet = S.fromList bound
            freeVars = S.difference freeInExpr boundSet
            freeVarsList = S.toList freeVars
        in freeVarsList

freeVarsExpr :: C.Expr -> S.Set C.TAloc
freeVarsExpr (C.CallFunc func env cont arg) = S.fromList [ (typ, aloc) | (typ, AAloc aloc) <- [func, env, cont, arg] ]
freeVarsExpr (C.CallCont cont env arg) = S.fromList [ (typ, aloc) | (typ, AAloc aloc) <- [cont, env, arg] ]
freeVarsExpr (C.Let aloc val body) = S.union (freeVarsValue val)
                                             (S.delete aloc (freeVarsExpr body))
freeVarsExpr (C.LetGlobalTuple _ elements body) =
        let freeElements = S.fromList [ (typ, aloc) | (typ, AAloc aloc) <- elements ]
            freeInBody = freeVarsExpr body
            freeVars = S.union freeElements freeInBody
        in freeVars
freeVarsExpr (C.If (C.RelOp op l r) c a) =
        let freePred = S.fromList [l, r]
            freeC = freeVarsExpr c
            freeA = freeVarsExpr a
            freeVars = S.unions [freePred, freeC, freeA]
        in freeVars

freeVarsValue :: C.Value -> S.Set C.TAloc
freeVarsValue (C.Int _) = S.empty
freeVarsValue (C.Bool _) = S.empty
freeVarsValue (C.VLabel _) = S.empty
freeVarsValue (C.Tuple elements) = S.fromList [ (typ, aloc) | (typ, AAloc aloc) <- elements ]
freeVarsValue (C.TupleRef tuple _) = S.fromList [ (typ, aloc) | (typ, AAloc aloc) <- [tuple] ]
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

replacePlaces :: M.Map Aloc Aloc -> [C.TAPlace] -> [C.TAPlace]
replacePlaces env places = map (replacePlace env) places

replacePlace :: M.Map Aloc Aloc -> C.TAPlace -> C.TAPlace
replacePlace env place@(typ, AAloc aloc) =
        case M.lookup aloc env of
             Nothing -> place
             Just aloc' -> (typ, AAloc aloc')
replacePlace _ place@(_, ALabel _) = place

replaceAlocs :: M.Map Aloc Aloc -> [C.TAloc] -> [C.TAloc]
replaceAlocs env places = map (replaceAloc env) places

replaceAloc :: M.Map Aloc Aloc -> C.TAloc -> C.TAloc
replaceAloc env tAloc@(typ, aloc) =
        case M.lookup aloc env of
             Nothing -> tAloc
             Just aloc' -> (typ, aloc')

regenAloc :: Aloc -> Closures Aloc
regenAloc (Aloc template _) = lift (genAloc template)
