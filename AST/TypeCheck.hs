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

instance Show Type where
         show TInt = showType TInt
         show TBool = showType TBool
         show typ@(TFunc _ _) = "(" ++ showType typ ++ ")"

showType :: Type -> String
showType TInt = "Int"
showType TBool = "Bool"
showType (TFunc from@(TFunc _ _) to) = unwords [ "(" ++ showType from ++ ")"
                                               , "->"
                                               , showType to
                                               ]
showType (TFunc from to) = unwords [ showType from
                                   , "->"
                                   , showType to
                                   ]

type Check a = ExceptT [CheckError] (State (M.Map Var Type)) a

execCheck :: Check a -> Either [CheckError] a
execCheck computation = evalState (runExceptT computation) M.empty

typeAnnToType :: TTypeAnn -> Type
typeAnnToType (_, TAInt) = TInt
typeAnnToType (_, TABool) = TBool
typeAnnToType (_, TAFunc from to) = TFunc (typeAnnToType from)
                                          (typeAnnToType to)

typeCheck :: Program -> Either [CheckError] T.Program
typeCheck (Program def) = execCheck ( typeCheckProgramDef def )

typeCheckProgramDef :: TProgramDef -> Check T.TProgramDef
typeCheckProgramDef (_, Main) = return ()
typeCheckProgramDef (_, LetDef def restDefs) =
        do
        typeCheckDef def
        let defName = extractDefName def
            defType = extractDefType def
        putVarType defName defType
        restDefs'@(typ, _) <- typeCheckProgramDef restDefs
        return (typ, C
typeCheckProgramDef (_, LetRecDefs defs restDefs) =
        do
        let defNames = map extractDefName defs
            defTypes = map extractDefType defs
        mapM (uncurry putVarType) (zip defNames defTypes)
        mapM typeCheckDef defs
        typeCheckProgramDef restDefs

typeCheckDef :: TDef -> Check ()
typeCheckDef def@(defPos, Def (_, Var "main") typeAnn _ _) =
        case typeAnnToType typeAnn of
             TInt -> typeCheckDef' def
             mainType -> throwError [ WithPos defPos ( unwords [ "Expected definition for"
                                                               , quote "main"
                                                               , "to have type"
                                                               , show TInt
                                                               , "but it is actually of type"
                                                               , show mainType
                                                               ]
                                                     )
                                    ]
typeCheckDef def = typeCheckDef' def

typeCheckDef' :: TDef -> Check ()
typeCheckDef' (_, Def (_, Var defName) typeAnn args body) =
        encapsulate ( do
                      let defType = typeAnnToType typeAnn
                      typeCheckArgsBody defName defType args body
                    )

typeCheckArgsBody :: String -> Type -> [TVar] -> TBody -> Check ()
typeCheckArgsBody defName expectedBodyType [] body@(bodyPos, _) =
        do
        maybeBodyType <- typeCheckBody body
        case maybeBodyType of
             Nothing -> return ()
             Just bodyType
                  | bodyType == expectedBodyType -> return ()
                  | otherwise -> tell [ WithPos bodyPos ( unwords [ "Body for definition of"
                                                                  , quote defName
                                                                  , "was expected to be of type"
                                                                  , show expectedBodyType
                                                                  , "but is actually of type"
                                                                  , show bodyType
                                                                  ]
                                                        )
                                      ]
typeCheckArgsBody defName (TFunc from to) ((_, arg) : rest) body =
        do
        putVarType arg from
        typeCheckArgsBody defName to rest body
typeCheckArgsBody defName _ ((argPos, _) : _) _ =
        tell [ WithPos argPos
                       ( unwords [ "Too many arguments in definition for"
                                 , defName
                                 ]
                       )
             ]

typeCheckBody :: TBody -> Check (Maybe Type)
typeCheckBody (_, Body expr) = typeCheckExpr expr

typeCheckExpr :: TExpr -> Check (Maybe Type)
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
                  | otherwise -> throw [ WithPos exprPos
                                                ( unwords [ "Left operand in use of binop"
                                                          , quote (binopPretty op)
                                                          , "was expected to be of type"
                                                          , show expectedLType
                                                          , "but is actually of type"
                                                          , show lType
                                                          ]
                                                )
                                      ]
        case maybeRType of
             Nothing -> return ()
             Just rType
                  | rType == expectedRType -> return ()
                  | otherwise -> tell [ WithPos exprPos
                                                ( unwords [ "Right operand in use of binop"
                                                          , quote (binopPretty op)
                                                          , "was expected to be of type"
                                                          , show expectedRType
                                                          , "but is actually of type"
                                                          , show rType
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
             (Just (TFunc expectedArgType retType), Just argType)
              | argType == expectedArgType -> return (Just retType)
              | otherwise -> do
                             tell [ WithPos exprPos
                                            ( unwords [ "Argument in function application was expected to be of type"
                                                      , show expectedArgType
                                                      , "but is actually of type"
                                                      , show argType
                                                      ]
                                            )
                                  ]
                             return Nothing
             (Just fType, _) -> do
                                tell [ WithPos exprPos
                                               ( unwords [ "Cannot use non-function of type"
                                                         , show fType
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
                      return ( maybeBodyType >>= return . TFunc varType )
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
             Just TBool -> return ()
             Just pType -> tell [ WithPos pPos
                                          ( unwords [ "The if expression predicated was expected to be of type"
                                                    , show TBool
                                                    , "but is actually of type"
                                                    , show pType
                                                    ]
                                          )
                                ]
        case (maybeCType, maybeAType) of
             (Nothing, _) -> return Nothing
             (_, Nothing) -> return Nothing
             (Just cType, Just aType)
              | cType == aType -> return (Just cType)
              | otherwise -> do
                             tell [ WithPos exprPos
                                            ( unwords [ "The if expression branches have different types."
                                                      , "The consequence is of type"
                                                      , show cType
                                                      , "while the alternative is of type"
                                                      , show aType
                                                      ]
                                            )
                                  ]
                             return Nothing

typeCheckValue :: TValue -> Check (Maybe Type)
typeCheckValue (_, Int _) = return (Just TInt)
typeCheckValue (_, Bool _) = return (Just TBool)
typeCheckValue (valPos, VVar (_, var@(Var varName))) =
        do
        maybeVarType <- lookupVarType var
        case maybeVarType of
             Nothing -> do
                        tell [ WithPos valPos
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

putVarType :: Var -> Type -> Check ()
putVarType var varType =
        do
        env <- lift get
        let env' = M.insert var varType env
        lift (put env')

lookupVarType :: Var -> Check (Maybe Type)
lookupVarType var =
        do
        env <- lift get
        return (M.lookup var env)

extractDefName :: TDef -> Var
extractDefName (_, Def (_, var) _ _ _) = var

extractDefType :: TDef -> Type
extractDefType (_, Def _ typeAnn _ _) = typeAnnToType typeAnn

quote :: String -> String
quote s = "\"" ++ s ++ "\""

opTypes :: BinOp -> (Type, Type, Type)
opTypes (BNumOp _) = (TInt, TInt, TInt)
opTypes (BRelOp _) = (TInt, TInt, TBool)

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
