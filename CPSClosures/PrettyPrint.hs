module CPSClosures.PrettyPrint
( prettyPrint
)
where

import Compiler.Types
import CPSClosures.Types

import Data.List

prettyPrint :: Program -> String
prettyPrint (Program defs body) = intercalate "\n" ( concat [ ppDefs defs
                                                            , [""]
                                                            , ppBody body
                                                            ]
                                                   )

ppDefs :: [Def] -> [String]
ppDefs defs = intercalate [""] (map ppDef defs)

ppDef :: Def -> [String]
ppDef (Func label env cont arg body) =
        unwords ["func", ppTLabel label, ppTLoc env, ppTLoc cont, ppTLoc arg, "="]
        : indent (ppBody body)
ppDef (Cont label env arg body) =
        unwords ["cont", ppTLabel label, ppTLoc env, ppTLoc arg, "="]
        : indent (ppBody body)

ppBody :: Body -> [String]
ppBody (Body expr) = ppExpr expr

ppExpr :: Expr -> [String]
ppExpr (CallFunc func env cont arg) = [ unwords (map ppTPlace [func, env, cont, arg]) ]
ppExpr (CallCont cont env arg) = [ unwords (map ppTPlace [cont, env, arg]) ]
ppExpr (Let loc val body) =
        unwords ["let", ppTLoc loc, "=", ppValue val]
        : ppExpr body
ppExpr (LetGlobalTuple label elements body) =
        unwords ["letglobaltuple", ppTLabel label, "=", ppElements ppTPlace elements]
        : ppExpr body
ppExpr (If p c a) =
        concat [ [ unwords [ "if", ppPred p, "then" ] ]
               , indent (ppExpr c)
               , ["else"]
               , indent (ppExpr a)
               ]

ppPred :: Pred -> String
ppPred (RelOp op l r) = unwords [ ppTLoc l, ppRelOp op, ppTLoc r ]

ppValue :: Value -> String
ppValue (Int i) = show i
ppValue (Bool True) = "true"
ppValue (Bool False) = "false"
ppValue (VLabel label) = ppTLabel label
ppValue (Tuple elements) = ppElements ppTPlace elements
ppValue (TupleRef place i) = concat [ "#", show i, " ", ppTPlace place ]
ppValue (NumOp op l r) = unwords [ ppTLoc l, ppNumOp op, ppTLoc r ]

ppType :: Type -> String
ppType (TFunc cont arg) = unwords [ ppType' cont, ppType' arg, "->", "⊥" ]
ppType (TCont arg) = unwords [ ppType' arg, "->", "⊥" ]
ppType typ = ppType' typ

ppType' :: Type -> String
ppType' TInt = "Int"
ppType' TBool = "Bool"
ppType' (TFunc cont arg) = parens (unwords [ ppType' cont, ppType' arg, "->", "⊥" ])
ppType' (TCont arg) = parens (unwords [ ppType' arg, "->", "⊥" ])
ppType' (TTuple elements) = ppElements ppType elements
ppType' (TClosure typ) = "<" ++ ppType typ ++ ">"

ppElements :: (a -> String) -> [a] -> String
ppElements pprinter elements = braces (intercalate ", " (map pprinter elements))

ppTLoc :: TAloc -> String
ppTLoc = ppTagged ppLoc

ppLoc :: Aloc -> String
ppLoc (Aloc template n) = template ++ "$" ++ show n

ppTLabel :: TLabel -> String
ppTLabel = ppTagged ppLabel

ppLabel :: Label -> String
ppLabel (Label template n) = template ++ "$" ++ show n
ppLabel HaltLabel = "halt"

ppTPlace :: TAPlace -> String
ppTPlace = ppTagged ppPlace

ppPlace :: APlace -> String
ppPlace (AAloc aloc) = ppLoc aloc
ppPlace (ALabel label) = ppLabel label

ppNumOp :: NumOp -> String
ppNumOp Add = "+"
ppNumOp Sub = "-"
ppNumOp Mul = "*"
ppNumOp Div = "/"
ppNumOp Mod = "%"

ppRelOp :: RelOp -> String
ppRelOp Lt = "<"
ppRelOp Gt = ">"
ppRelOp Eq = "=="
ppRelOp Lte = "<="
ppRelOp Gte = ">="
ppRelOp Neq = "/="

ppTagged :: (a -> String) -> Tagged a -> String
ppTagged pprinter (typ, a) = parens (unwords [pprinter a, ":", ppType typ])

indent :: [String] -> [String]
indent lines = map ("    " ++) lines

parens :: String -> String
parens s = "(" ++ s ++ ")"

braces :: String -> String
braces s = "{" ++ s ++ "}"
