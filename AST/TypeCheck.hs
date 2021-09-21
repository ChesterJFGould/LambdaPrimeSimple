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

prettyPrintType :: T.Type -> String
prettyPrintType T.TInt = "Int"
prettyPrintType T.TBool = "Bool"
prettyPrintType (T.TFunc from@(T.TFunc _ _) to) = unwords [ "(" ++ prettyPrintType from ++ ")"
                                                          , "->"
                                                          , prettyPrintType to
                                                          ]
prettyPrintType (T.TFunc from to) = unwords [ prettyPrintType from
                                            , "->"
                                            , prettyPrintType to
                                            ]

type Check a = ExceptT [CheckError] (State (M.Map Var T.Type)) a

execCheck :: Check a -> Either [CheckError] a
execCheck computation = evalState (runExceptT computation) M.empty

typeAnnToType :: TTypeAnn -> T.Type
typeAnnToType (_, TAInt) = T.TInt
typeAnnToType (_, TABool) = T.TBool
typeAnnToType (_, TAFunc from to) = T.TFunc (typeAnnToType from)
                                            (typeAnnToType to)

typeCheck :: Program -> Either [CheckError] T.Program
typeCheck (Program def) = T.Program <$> execCheck (typeCheckProgramDef def)

typeCheckProgramDef :: TProgramDef -> Check T.TProgramDef
typeCheckProgramDef (loc, Main) = return (loc, T.TInt, T.Main)
typeCheckProgramDef (loc, LetDef def restDefs) =
        do
        def' <- typeCheckDef def
        let defName = extractDefName def
            defType = extractDefType def
        putVarType defName defType
        restDefs'@(_, typ, _) <- typeCheckProgramDef restDefs
        return (loc, typ, T.LetDef def' restDefs')
typeCheckProgramDef (defsLoc, LetRecDefs defs restDefs) =
        do
        let defNames = map extractDefName defs
            defTypes = map extractDefType defs
        mapM (uncurry putVarType) (zip defNames defTypes)
        defs' <- mapM typeCheckDef defs
        restDefs'@(_, typ, _) <- typeCheckProgramDef restDefs
        return (defsLoc, typ, T.LetRecDefs defs' restDefs')

typeCheckDef :: TDef -> Check T.Def
typeCheckDef def@(defPos, Def (_, Var "main") typeAnn _ _) =
        case typeAnnToType typeAnn of
             T.TInt -> typeCheckDef' def
             mainType -> throwError [ WithPos defPos ( unwords [ "Expected definition for"
                                                               , quote "main"
                                                               , "to have type"
                                                               , prettyPrintType T.TInt
                                                               , "but it is actually of type"
                                                               , prettyPrintType mainType
                                                               ]
                                                     )
                                    ]
typeCheckDef def = typeCheckDef' def

typeCheckDef' :: TDef -> Check T.Def
typeCheckDef' (_, Def defName typeAnn args body) =
        encapsulate ( do
                      let defType = typeAnnToType typeAnn
                      typeCheckArgsBody defName defType args body
                    )

typeCheckArgsBody :: TVar -> T.Type -> [TVar] -> TBody -> Check T.Def
typeCheckArgsBody (nameLoc, Var defName) expectedBodyType [] body@(bodyPos, _) =
        do
        body'@(_, bodyType, _) <- typeCheckBody body
        if bodyType == expectedBodyType
        then return (T.Def (nameLoc, bodyType, T.Var defName) [] body')
        else throwError [ WithPos bodyPos ( unwords [ "Body for definition of"
                                                    , quote defName
                                                    , "was expected to be of type"
                                                    , prettyPrintType expectedBodyType
                                                    , "but is actually of type"
                                                    , prettyPrintType bodyType
                                                    ]
                                          )
                        ]
typeCheckArgsBody defName (T.TFunc from to) ((argLoc, arg@(Var argName)) : rest) body =
        do
        putVarType arg from
        (T.Def defName' args' body') <- typeCheckArgsBody defName to rest body
        return (T.Def defName' ((argLoc, from, T.Var argName) : args') body')
typeCheckArgsBody (_, Var defName) _ ((argPos, _) : _) _ =
        throwError [ WithPos argPos
                             ( unwords [ "Too many arguments in definition for"
                                       , defName
                                       ]
                             )
                   ]

typeCheckBody :: TBody -> Check T.TBody
typeCheckBody (bodyLoc, Body expr) = raiseTagged bodyLoc T.Body <$> typeCheckExpr expr

typeCheckExpr :: TExpr -> Check T.TExpr
typeCheckExpr (valueLoc, Value value) = raiseTagged valueLoc T.Value <$> typeCheckValue value
typeCheckExpr (exprLoc, BinOp op l r) =
        do
        let (expectedLType, expectedRType, retType) = opTypes op
        (l'@(_, lType, _), r'@(_, rType, _)) <- combineErrors (typeCheckExpr l)
                                                              (typeCheckExpr r)
        if lType /= expectedLType
        then throwError [ WithPos exprLoc
                                  ( unwords [ "Left operand in use of binop"
                                            , quote (binopPretty op)
                                            , "was expected to be of type"
                                            , prettyPrintType expectedLType
                                            , "but is actually of type"
                                            , prettyPrintType lType
                                            ]
                                  )
                        ]
        else if rType /= expectedRType
        then throwError [ WithPos exprLoc
                                  ( unwords [ "Right operand in use of binop"
                                            , quote (binopPretty op)
                                            , "was expected to be of type"
                                            , prettyPrintType expectedRType
                                            , "but is actually of type"
                                            , prettyPrintType rType
                                            ]
                                  )
                        ]
        else return (exprLoc, retType, T.BinOp op l' r')
typeCheckExpr (exprLoc, Apply f arg) =
        do
        (f'@(_, fType, _), arg'@(_, argType, _)) <- combineErrors (typeCheckExpr f)
                                                                  (typeCheckExpr arg)
        case fType of
             (T.TFunc expectedArgType retType)
              | argType == expectedArgType -> return (exprLoc, retType, T.Apply f' arg')
              | otherwise -> throwError [ WithPos exprLoc
                                                  ( unwords [ "Argument in function application was expected to be of type"
                                                            , prettyPrintType expectedArgType
                                                            , "but is actually of type"
                                                            , prettyPrintType argType
                                                            ]
                                                  )
                                        ]
             _ -> throwError [ WithPos exprLoc
                                       ( unwords [ "Cannot use non-function of type"
                                                 , prettyPrintType fType
                                                 , "as the function in a function application"
                                                 ]
                                       )
                             ]
typeCheckExpr (exprLoc, Lambda (varPos, var@(Var varName)) typeAnn body) =
        encapsulate ( do
                      let varType = typeAnnToType typeAnn
                      putVarType var varType
                      body'@(_, bodyType, _) <- typeCheckExpr body
                      return (exprLoc, T.TFunc varType bodyType, T.Lambda (varPos, varType, T.Var varName) body')
                    )
typeCheckExpr (exprLoc, Let (varLoc, var@(Var varName)) val body) =
        encapsulate ( do
                      val'@(_, valType, _) <- typeCheckExpr val
                      putVarType var valType
                      body'@(_, bodyType, _) <- typeCheckExpr body
                      return (exprLoc, bodyType, T.Let (varLoc, valType, T.Var varName) val' body')
                    )
typeCheckExpr (exprLoc, If p@(pPos, _) c a) =
        do
        (p'@(_, pType, _), (c'@(_, cType, _), a'@(_, aType, _))) <-
                combineErrors (typeCheckExpr p)
                              (combineErrors (typeCheckExpr c)
                                       (typeCheckExpr a))
        case pType of
             T.TBool
              | cType == aType -> return (exprLoc, cType, T.If p' c' a')
              | otherwise -> throwError [ WithPos exprLoc
                                                  ( unwords [ "The if expression branches have different types."
                                                            , "The consequence is of type"
                                                            , prettyPrintType cType
                                                            , "while the alternative is of type"
                                                            , prettyPrintType aType
                                                            ]
                                                  )
                                        ]
             _ -> throwError [ WithPos pPos
                                       ( unwords [ "The if expression predicate was expected to be of type"
                                                 , prettyPrintType T.TBool
                                                 , "but is actually of type"
                                                 , prettyPrintType pType
                                                 ]
                                       )
                             ]

typeCheckValue :: TValue -> Check T.TValue
typeCheckValue (valueLoc, Int i) = return (valueLoc, T.TInt, T.Int i)
typeCheckValue (valueLoc, Bool b) = return (valueLoc, T.TBool, T.Bool b)
typeCheckValue (valueLoc, VVar (varPos, var@(Var varName))) =
        do
        maybeVarType <- lookupVarType var
        case maybeVarType of
             Nothing -> throwError [ WithPos valueLoc
                                             ( unwords [ "Undefined variable"
                                                       , varName
                                                       ]
                                             )
                                   ]
             Just varType -> return (raiseTagged valueLoc T.VVar (varPos, varType, T.Var varName))

raiseTagged :: FileLocation -> (T.Tagged a -> b) -> T.Tagged a -> T.Tagged b
raiseTagged loc cons val@(_, typ, _) = (loc, typ, cons val)

combineErrors :: Check a -> Check b -> Check (a, b)
combineErrors computationA computationB =
        do
        resultA <- catchError computationA
                              (\errA ->
                               do
                               catchError computationB
                                          (\errB ->
                                           throwError (errA ++ errB)
                                          )
                               throwError errA
                              )
        resultB <- computationB
        return (resultA, resultB)

encapsulate :: Check a -> Check a
encapsulate computation =
        do
        env <- lift get
        result <- computation
        lift (put env)
        return result

putVarType :: Var -> T.Type -> Check ()
putVarType var varType =
        do
        env <- lift get
        let env' = M.insert var varType env
        lift (put env')

lookupVarType :: Var -> Check (Maybe T.Type)
lookupVarType var =
        do
        env <- lift get
        return (M.lookup var env)

extractDefName :: TDef -> Var
extractDefName (_, Def (_, var) _ _ _) = var

extractDefType :: TDef -> T.Type
extractDefType (_, Def _ typeAnn _ _) = typeAnnToType typeAnn

quote :: String -> String
quote s = "\"" ++ s ++ "\""

opTypes :: BinOp -> (T.Type, T.Type, T.Type)
opTypes (BNumOp _) = (T.TInt, T.TInt, T.TInt)
opTypes (BRelOp _) = (T.TInt, T.TInt, T.TBool)

binopPretty :: BinOp -> String
binopPretty (BNumOp Add) = "+"
binopPretty (BNumOp Sub) = "-"
binopPretty (BNumOp Mul) = "*"
binopPretty (BRelOp Lt) = "<"
binopPretty (BRelOp Gt) = ">"
binopPretty (BRelOp Eq) = "=="
binopPretty (BRelOp Lte) = "<="
binopPretty (BRelOp Gte) = ">="
binopPretty (BRelOp Neq) = "/="
