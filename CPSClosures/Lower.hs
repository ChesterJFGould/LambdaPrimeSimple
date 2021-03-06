module CPSClosures.Lower
( lower
)
where

import Compiler.Gensym
import qualified Compiler.Locations as L
import Compiler.Types
import qualified CPSAsm.Types as A
import CPSClosures.Types

import Control.Monad.Writer

type Tuples a = WriterT [GlobalTuple] (Gensym) a

runTuples :: Tuples a -> Gensym (a, [GlobalTuple])
runTuples computation = runWriterT computation

lower :: Program -> Gensym A.Program
lower (Program defs body) =
        do
        ((blocks, body'), globalTuples) <- runTuples ( do
                                                       defs' <- lowerDefs defs
                                                       body' <- lowerProgBody body
                                                       return (defs', body')
                                                     )
        return (A.Program globalTuples blocks body')

lowerDefs :: [Def] -> Tuples [A.Block]
lowerDefs = mapM lowerDef

lowerDef :: Def -> Tuples A.Block
lowerDef (Func label env cont arg (Body expr)) =
        do
        let label' = eraseType label
            env' = eraseType env
            cont' = eraseType cont
            arg' = eraseType arg
        expr' <- setExprs [ (MAloc env', regToMTriv L.envRegister)
                          , (MAloc cont', regToMTriv L.contRegister)
                          , (MAloc arg', regToMTriv L.argRegister)
                          ] <$> lowerExpr expr
        let body = A.Body [ L.envRegister, L.contRegister, L.argRegister, L.heapRegister] expr'
        return (A.Block label' body)
lowerDef (Cont label env arg (Body expr)) =
        do
        let label' = eraseType label
            env' = eraseType env
            arg' = eraseType arg
        expr' <- setExprs [ (MAloc env', regToMTriv L.envRegister)
                          , (MAloc arg', regToMTriv L.argRegister)
                          ] <$> lowerExpr expr
        let body = A.Body [ L.envRegister, L.argRegister] expr'
        return (A.Block label' body)

lowerExpr :: Expr -> Tuples A.Expr
lowerExpr (CallFunc func env cont arg) =
        do
        let func' = eraseType func
            env' = eraseType env
            cont' = eraseType cont
            arg' = eraseType arg
        return ( setExprs [ (regToMloc L.envRegister, aPlaceToMTriv env')
                          , (regToMloc L.contRegister, aPlaceToMTriv cont')
                          , (regToMloc L.argRegister, aPlaceToMTriv arg')
                          ] (A.Jump (aPlaceToMPlace func')
                                    [L.envRegister, L.contRegister, L.argRegister, L.heapRegister])
               )
lowerExpr (CallCont func env arg) =
        do
        let func' = eraseType func
            env' = eraseType env
            arg' = eraseType arg
        return ( setExprs [ (regToMloc L.envRegister, aPlaceToMTriv env')
                          , (regToMloc L.argRegister, aPlaceToMTriv arg')
                          ] (A.Jump (aPlaceToMPlace func')
                                    [L.envRegister, L.argRegister, L.heapRegister])
               )
lowerExpr (Let aloc (Int i) body) =
        do
        let aloc' = eraseType aloc
        A.Set (MAloc aloc') (MWord (fromIntegral i))
              <$> lowerExpr body
lowerExpr (Let aloc (Bool True) body) =
        do
        let aloc' = eraseType aloc
        A.Set (MAloc aloc') (MWord 1)
              <$> lowerExpr body
lowerExpr (Let aloc (Bool False) body) =
        do
        let aloc' = eraseType aloc
        A.Set (MAloc aloc') (MWord 0)
              <$> lowerExpr body
lowerExpr (Let aloc (VLabel label) body) =
        do
        let aloc' = eraseType aloc
            label' = eraseType label
        A.Set (MAloc aloc') (MPlace (MLabel label'))
              <$> lowerExpr body
lowerExpr (Let aloc (Tuple elements) body) = -- TODO: This can be made nicer
        do
        let aloc' = eraseType aloc
        body' <- lowerExpr body
        offsetMloc <- MAloc <$> lift (genAloc "offset")
        let tupleBody = A.Set (MAloc aloc') (regToMTriv L.heapRegister)
                              (A.Set offsetMloc (MWord (fromIntegral ((length elements) * 8)))
                                     (A.NumOp Add (regToMloc L.heapRegister) offsetMloc
                                              body'))
            elements' = map eraseType elements
        return (initializeElements (MMloc (MReg L.heapRegister)) (zip elements' [0..]) tupleBody)
lowerExpr (Let aloc (TupleRef place offset) body) =
        do
        let aloc' = eraseType aloc
            place' = eraseType place
        A.MRef (MAloc aloc') (aPlaceToMPlace place') (offset * 8)
               <$> lowerExpr body
lowerExpr (Let aloc (NumOp op l r) body) =
        do
        body' <- lowerExpr body
        let aloc' = eraseType aloc
            l' = eraseType l
            r' = eraseType r
        return ( A.Set (MAloc aloc') (alocToMTriv l')
                       (A.NumOp op (MAloc aloc') (MAloc r') body')
               )
lowerExpr (LetGlobalTuple label elements body) =
        do
        let initialValues = replicate (length elements) (TWord 0)
            label' = eraseType label
        tell [ GlobalTuple label' initialValues ]
        let elements' = map eraseType elements
        initializeElements (MLabel label') (zip elements' [0..])
                           <$> lowerExpr body
lowerExpr (If (RelOp op l r) c a) =
        do
        let l' = eraseType l
            r' = eraseType r
        A.If (A.RelOp op (MAloc l') (MAloc r'))
             <$> lowerExpr c
             <*> lowerExpr a

initializeElements :: MPlace -> [(APlace, Int)] -> A.Expr -> A.Expr
initializeElements _ [] body = body
initializeElements tuple ((element, offset) : rest) body =
        A.MSet tuple (offset * 8) (aPlaceToMPlace element)
               (initializeElements tuple rest body)

lowerProgBody :: Body -> Tuples A.Body
lowerProgBody (Body expr) = A.Body [] <$> lowerExpr expr

setExprs :: [(Mloc, MTriv)] -> A.Expr -> A.Expr
setExprs [] body = body
setExprs ((mloc, triv) : rest) body = A.Set mloc triv (setExprs rest body)

regToMloc :: Reg -> Mloc
regToMloc = MReg

regToMTriv :: Reg -> MTriv
regToMTriv = MPlace . regToMPlace

regToMPlace :: Reg -> MPlace
regToMPlace = MMloc . regToMloc

alocToMTriv :: Aloc -> MTriv
alocToMTriv = MPlace . MMloc . MAloc

aPlaceToMTriv :: APlace -> MTriv
aPlaceToMTriv = MPlace . aPlaceToMPlace

aPlaceToMPlace :: APlace -> MPlace
aPlaceToMPlace (AAloc aloc) = MMloc (MAloc aloc)
aPlaceToMPlace (ALabel label) = MLabel label

eraseType :: Tagged a -> a
eraseType (_, a) = a
