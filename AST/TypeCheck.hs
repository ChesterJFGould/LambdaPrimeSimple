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
typeCheckProgramDef (_, LetRecDefs defs restDefs) =
        do
        let defNames = map extractDefName defs
            defTypes = map extractDefType defs
        mapM (uncurry putVarType) (zip defNames defTypes)
        mapM typeCheckDef defs
        typeCheckProgramDef restDefs

typeCheckDef :: TDef -> Check T.TDef
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
typeCheckDef' (_, Def (_, Var defName) typeAnn args body) =
        encapsulate ( do
                      let defType = typeAnnToType typeAnn
                      typeCheckArgsBody defName defType args body
                    )

typeCheckArgsBody :: TVar -> T.Type -> [TVar] -> TBody -> Check T.Def
typeCheckArgsBody defName expectedBodyType [] body@(bodyPos, _) =
        do
        maybeBodyType <- typeCheckBody body
        case maybeBodyType of
             Nothing -> return ()
             Just bodyType
                  | bodyType == expectedBodyType -> return (T.Def
                  | otherwise -> throwError [ WithPos bodyPos ( unwords [ "Body for definition of"
                                                                        , quote defName
                                                                        , "was expected to be of type"
                                                                        , prettyPrintType expectedBodyType
                                                                        , "but is actually of type"
                                                                        , prettyPrintType bodyType
                                                                        ]
                                                              )
                                            ]
typeCheckArgsBody defName (T.TFunc from to) ((_, arg) : rest) body =
        do
        putVarType arg from
        typeCheckArgsBody defName to rest body
typeCheckArgsBody defName _ ((argPos, _) : _) _ =
        throwError [ WithPos argPos
                             ( unwords [ "Too many arguments in definition for"
                                       , defName
                                       ]
                             )
                   ]

typeCheckBody :: TBody -> Check (Maybe T.Type)
typeCheckBody (_, Body expr) = typeCheckExpr expr

typeCheckExpr :: TExpr -> Check (Maybe T.Type)
typeCheckExpr (_, Value value) = typeCheckValue value
typeCheckExpr (exprPos, BinOp op l r) =
        do
        let (expectedLType, expectedRType, retType) = opTypes op
        maybeLType <- typeCheckExpr l
        maybeRType <- typeCheckExpr r
        case maybeLType of
             Nothing -> return ()
             Just lType
                  | lType == expectedLType -> return ()
                  | otherwise -> throwError [ WithPos exprPos
                                                      ( unwords [ "Left operand in use of binop"
                                                                , quote (binopPretty op)
                                                                , "was expected to be of type"
                                                                , prettyPrintType expectedLType
                                                                , "but is actually of type"
                                                                , prettyPrintType lType
                                                                ]
                                                      )
                                            ]
        case maybeRType of
             Nothing -> return ()
             Just rType
                  | rType == expectedRType -> return ()
                  | otherwise -> throwError [ WithPos exprPos
                                                      ( unwords [ "Right operand in use of binop"
                                                                , quote (binopPretty op)
                                                                , "was expected to be of type"
                                                                , prettyPrintType expectedRType
                                                                , "but is actually of type"
                                                                , prettyPrintType rType
                                                                ]
                                                      )
                                            ]
        return (Just retType)
typeCheckExpr (exprPos, Apply f arg) =
        do
        maybeFType <- typeCheckExpr f
        maybeArgType <- typeCheckExpr arg
        case (maybeFType, maybeArgType) of
             (Nothing, _) -> return Nothing
             (_, Nothing) -> return Nothing
             (Just (T.TFunc expectedArgType retType), Just argType)
              | argType == expectedArgType -> return (Just retType)
              | otherwise -> do
                             throwError [ WithPos exprPos
                                                  ( unwords [ "Argument in function application was expected to be of type"
                                                            , prettyPrintType expectedArgType
                                                            , "but is actually of type"
                                                            , prettyPrintType argType
                                                            ]
                                                  )
                                        ]
                             return Nothing
             (Just fType, _) -> do
                                throwError [ WithPos exprPos
                                                     ( unwords [ "Cannot use non-function of type"
                                                               , prettyPrintType fType
                                                               , "as the function in a function application"
                                                               ]
                                                     )
                                           ]
                                return Nothing
typeCheckExpr (exprPos, Lambda (_, var) typeAnn body) =
        encapsulate ( do
                      let varType = typeAnnToType typeAnn
                      putVarType var varType
                      maybeBodyType <- typeCheckExpr body
                      return ( maybeBodyType >>= return . T.TFunc varType )
                    )
typeCheckExpr (exprPos, Let (_, var) val body) =
        encapsulate ( do
                      maybeValType <- typeCheckExpr val
                      case maybeValType of
                           Nothing -> return Nothing
                           Just valType -> do
                                           putVarType var valType
                                           typeCheckExpr body
                    )
typeCheckExpr (exprPos, If p@(pPos, _) c a) =
        do
        maybePType <- typeCheckExpr p
        maybeCType <- typeCheckExpr c
        maybeAType <- typeCheckExpr a
        case maybePType of
             Nothing -> return ()
             Just T.TBool -> return ()
             Just pType -> throwError [ WithPos pPos
                                                ( unwords [ "The if expression predicated was expected to be of type"
                                                          , prettyPrintType T.TBool
                                                          , "but is actually of type"
                                                          , prettyPrintType pType
                                                          ]
                                                )
                                      ]
        case (maybeCType, maybeAType) of
             (Nothing, _) -> return Nothing
             (_, Nothing) -> return Nothing
             (Just cType, Just aType)
              | cType == aType -> return (Just cType)
              | otherwise -> do
                             throwError [ WithPos exprPos
                                                  ( unwords [ "The if expression branches have different types."
                                                            , "The consequence is of type"
                                                            , prettyPrintType cType
                                                            , "while the alternative is of type"
                                                            , prettyPrintType aType
                                                            ]
                                                  )
                                        ]
                             return Nothing

typeCheckValue :: TValue -> Check (Maybe T.Type)
typeCheckValue (_, Int _) = return (Just T.TInt)
typeCheckValue (_, Bool _) = return (Just T.TBool)
typeCheckValue (valPos, VVar (_, var@(Var varName))) =
        do
        maybeVarType <- lookupVarType var
        case maybeVarType of
             Nothing -> do
                        throwError [ WithPos valPos
                                             ( unwords [ "Undefined variable"
                                                       , varName
                                                       ]
                                             )
                                   ]
                        return Nothing
             Just varType -> return (Just varType)

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
