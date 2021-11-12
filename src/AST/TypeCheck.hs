module AST.TypeCheck
( typeCheck
)
where

import AST.Types
import Compiler.Types
import qualified TAST.Types as T

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as M

type Check a = ExceptT [CheckError] (State (M.Map Var T.Type)) a

execCheck :: Check a -> Either [CheckError] a
execCheck computation = evalState (runExceptT computation) M.empty

typeCheck :: Program -> Either [CheckError] T.Program
typeCheck (Program def) = T.Program <$> execCheck ( do
                                                    def' <- typeCheckProgramDef def
                                                    checkMain
                                                    return def'
                                                  )

checkMain :: Check ()
checkMain =
        do
        maybeMainT <- lookupVarType (Var "main")
        case maybeMainT of
             (Just T.TInt) -> return ()
             (Just mainT) -> errorWithoutPos [ "Expected definition for"
                                             , quote "main"
                                             , "to be of type"
                                             , prettyPrintType T.TInt ++ ","
                                             , "but instead it has type"
                                             , prettyPrintType mainT
                                             ]
             Nothing -> errorWithoutPos [ "No definition for main" ]

typeCheckProgramDef :: TProgramDef -> Check T.TProgramDef
typeCheckProgramDef (loc, Main) = return (loc, T.TInt, T.Main)
typeCheckProgramDef (loc, LetValue value def) =
        do
        value' <- typeCheckValue value
        putVarType (valueName value) (valueType value)
        def' <- typeCheckProgramDef def
        return (loc, tType def', T.LetValue value' def')
typeCheckProgramDef (loc, LetFuncs funcs def) =
        do
        mapM (uncurry putVarType) (zip (map funcName funcs) (map funcType funcs))
        funcs' <- mapM typeCheckFunc funcs
        def' <- typeCheckProgramDef def
        return (loc, tType def', T.LetFuncs funcs' def')

typeCheckValue :: TValue -> Check T.TValue
typeCheckValue (loc, Value var@(_, name) typeAnn body) =
        do
        let expectedType = typeAnnToType typeAnn
        body' <- typeCheckBody body
        if expectedType == tType body'
        then do
             let typ = tType body'
                 var' = giveType typ var
             return (loc, typ, T.Value var' body')
        else errorWithPos (tLoc body') [ "Value of"
                                       , varString name
                                       , "was declared to have type"
                                       , quote (prettyPrintType expectedType)
                                       , ", but instead it has type"
                                       , quote (prettyPrintType (tType body'))
                                       ]

typeCheckFunc :: TFunc -> Check T.TFunc
typeCheckFunc func@(_, Func _ typeAnn _ _) =
        do
        let expectedType = typeAnnToType typeAnn
        encapsulate (typeCheckFunc' expectedType func)

typeCheckFunc' :: T.Type -> TFunc -> Check T.TFunc
typeCheckFunc' expectedType (loc, Func var@(_, name) _ [] body) =
        do
        body' <- typeCheckBody body
        if expectedType == tType body'
        then do
             let typ = tType body'
                 var' = giveType typ var
             return (loc, typ, T.Func var' [] body')
        else errorWithPos (tLoc body') [ "Body of"
                                       , quote (varString name)
                                       , "was declared to have type"
                                       , quote (prettyPrintType expectedType)
                                       , "but it instead has type"
                                       , quote (prettyPrintType (tType body'))
                                       ]
typeCheckFunc' (T.TFunc fromT toT) (loc, Func var typeAnn (arg@(argLoc, argName) : rest) body) =
        do
        putVarType argName fromT
        (loc, typ, T.Func var args body) <- typeCheckFunc' toT (loc, Func var typeAnn rest body)
        return (loc, T.TFunc fromT typ, T.Func var ((argLoc, fromT, argName) : args) body)
typeCheckFunc' _ (loc, Func var@(_, name) _ _ _) =
        errorWithPos loc [ "Definition of"
                         , quote (varString name)
                         , "has too many arguments with respect to its declared type"
                         ]

typeCheckBody :: TBody -> Check T.TBody
typeCheckBody (loc, Body expr) =
        do
        expr' <- typeCheckExpr expr
        return (loc, tType expr', T.Body expr')

typeCheckExpr :: TExpr -> Check T.TExpr
typeCheckExpr (loc, Atom atom) =
        do
        atom' <- typeCheckAtom atom
        return (loc, tType atom', T.Atom atom')
typeCheckExpr (loc, BinOp op l r) =
        do
        let (expectedLType, expectedRType, returnType) = binOpTypes op
        l' <- typeCheckExpr l
        r' <- typeCheckExpr r
        if expectedLType == tType l'
        then if expectedRType == tType r'
             then return (loc, returnType, T.BinOp op l' r')
             else errorWithPos (tLoc r') [ "Expected right-hand argument of"
                                         , quote (prettyPrintBinOp op)
                                         , "to be of type"
                                         , quote (prettyPrintType expectedRType) ++ ","
                                         , "but instead it is of type"
                                         , quote (prettyPrintType (tType r'))
                                         ]
        else errorWithPos (tLoc l') [ "Expected left-hand argument of"
                                    , quote (prettyPrintBinOp op)
                                    , "to be of type"
                                    , quote (prettyPrintType expectedLType)
                                    , ". Instead it is of type"
                                    , quote (prettyPrintType (tType l'))
                                    ]
typeCheckExpr (loc, Apply f arg) =
        do
        (f', arg') <- combineErrors (typeCheckExpr f) (typeCheckExpr arg)
        case (tType f', tType arg') of
             (T.TFunc expectedArgT toT, argT)
              | expectedArgT == argT -> return (loc, toT, T.Apply f' arg')
              | otherwise -> errorWithPos (tLoc arg') [ "Expected function argument to be of type"
                                                      , quote (prettyPrintType expectedArgT) ++ ","
                                                      , "but instead it is of type"
                                                      , quote (prettyPrintType argT)
                                                      ]
             (fType, _) -> errorWithPos (tLoc f') [ "Cannot use expression of non-function type"
                                                  , quote (prettyPrintType fType)
                                                  , "in function application"
                                                  ]
typeCheckExpr (loc, Let (varLoc, var) val body) =
        do
        val' <- typeCheckExpr val
        encapsulate ( do
                      putVarType var (tType val')
                      body' <- typeCheckExpr body
                      return (loc, tType body', T.Let (varLoc, tType val', var) val' body')
                    )
typeCheckExpr (loc, If p c a) =
        do
        (p', (c', a')) <- combineErrors (typeCheckExpr p)
                                        (combineErrors (typeCheckExpr c)
                                                       (typeCheckExpr a))
        case (tType p', tType c', tType a') of
             (T.TBool, cType, aType)
              | cType == aType -> return (loc, cType, T.If p' c' a')
              | otherwise -> errorWithPos loc [ "If expression branches were"
                                              , "expected to be of the same type,"
                                              , "but instead are of type"
                                              , quote (prettyPrintType cType)
                                              , "and"
                                              , quote (prettyPrintType aType)
                                              , "respectively"
                                              ]
             (pType, _, _) -> errorWithPos (tLoc p') [ "If predicate was expected to be of type"
                                                     , quote (prettyPrintType T.TBool) ++ ","
                                                     , "but instead it is of type"
                                                     , quote (prettyPrintType pType)
                                                     ]

typeCheckAtom :: TAtom -> Check T.TAtom
typeCheckAtom (loc, Int i) = return (loc, T.TInt, T.Int i)
typeCheckAtom (loc, Bool b) = return (loc, T.TBool, T.Bool b)
typeCheckAtom (loc, VVar (varLoc, var)) =
        do
        maybeVarT <- lookupVarType var
        case maybeVarT of
             Just varT -> return (loc, varT, T.VVar (varLoc, varT, var))
             Nothing -> errorWithPos varLoc [ "Undefined variable"
                                            , quote (varString var)
                                            ]

putVarType :: Var -> T.Type -> Check ()
putVarType var typ =
        do
        env <- lift get
        let env' = M.insert var typ env
        lift (put env')
        return ()

lookupVarType :: Var -> Check (Maybe T.Type)
lookupVarType var =
        do
        env <- lift get
        return (M.lookup var env)

valueName :: TValue -> Var
valueName (_, Value (_, var) _ _) = var

valueType :: TValue -> T.Type
valueType (_, Value _ typeAnn _) = typeAnnToType typeAnn

funcName :: TFunc -> Var
funcName (_, Func (_, var) _ _ _) = var

funcType :: TFunc -> T.Type
funcType (_, Func _ typeAnn _ _) = typeAnnToType typeAnn

tType :: T.Tagged a -> T.Type
tType (_, typ, _) = typ

tLoc :: T.Tagged a -> FileLocation
tLoc (loc, _, _) = loc

typeAnnToType :: TTypeAnn -> T.Type
typeAnnToType (_, TAInt) = T.TInt
typeAnnToType (_, TABool) = T.TBool
typeAnnToType (_, TAFunc f t) = T.TFunc (typeAnnToType f) (typeAnnToType t)

giveType :: T.Type -> Tagged a -> T.Tagged a
giveType typ (loc, a) = (loc, typ, a)

errorWithPos :: FileLocation -> [String] -> Check a
errorWithPos loc words = throwError [WithPos loc (unwords words)]

errorWithoutPos :: [String] -> Check a
errorWithoutPos words = throwError [WithoutPos (unwords words)]

varString :: Var -> String
varString (Var s) = s

prettyPrintType :: T.Type -> String
prettyPrintType T.TInt = "Int"
prettyPrintType T.TBool = "Bool"
prettyPrintType (T.TFunc f@(T.TFunc _ _) t) = unwords [ parens (prettyPrintType f), "->", prettyPrintType t ]
prettyPrintType (T.TFunc f t) = unwords [ prettyPrintType f, "->", prettyPrintType t ]

prettyPrintBinOp :: BinOp -> String
prettyPrintBinOp (Num Add) = "+"
prettyPrintBinOp (Num Sub) = "-"
prettyPrintBinOp (Num Mul) = "*"
prettyPrintBinOp (Num Div) = "/"
prettyPrintBinOp (Num Mod) = "%"
prettyPrintBinOp (Rel Lt) = "<"
prettyPrintBinOp (Rel Gt) = ">"
prettyPrintBinOp (Rel Eq) = "=="
prettyPrintBinOp (Rel Lte) = "<="
prettyPrintBinOp (Rel Gte) = ">="
prettyPrintBinOp (Rel Neq) = "/="

encapsulate :: Check a -> Check a
encapsulate computation =
        do
        env <- lift get
        result <- computation
        lift (put env)
        return result

quote :: String -> String
quote s = "\"" ++ s ++ "\""

parens :: String -> String
parens s = "(" ++ s ++ ")"

combineErrors :: Check a -> Check b -> Check (a, b)
combineErrors computationA computationB =
        do
        resultA <- catchError computationA
                              (\errA -> 
                               do
                               catchError computationB
                                          (\errB -> throwError (errA ++ errB))
                               throwError errA
                              )
        resultB <- computationB
        return (resultA, resultB)

binOpTypes :: BinOp -> (T.Type, T.Type, T.Type)
binOpTypes (Num _) = (T.TInt, T.TInt, T.TInt)
binOpTypes (Rel _) = (T.TInt, T.TInt, T.TBool)
