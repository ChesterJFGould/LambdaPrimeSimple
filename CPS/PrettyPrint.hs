module CPS.PrettyPrint
( prettyPrint
)
where

import Compiler.Types
import CPS.Types

import Data.List

prettyPrint :: Program -> String
prettyPrint (Program expr) = intercalate "\n" (prettyPrintExpr expr)

prettyPrintExpr :: Expr -> [String]
prettyPrintExpr (CallFunc f cont arg) =
        [unwords [prettyPrintTPlace f, prettyPrintTPlace cont, prettyPrintTPlace arg]]
prettyPrintExpr (CallCont cont arg) =
        [unwords [prettyPrintTPlace cont, prettyPrintTPlace arg]]
prettyPrintExpr (Let aloc val body) =
        unwords ["let", prettyPrintTLoc aloc, "=", prettyPrintValue val]
        : prettyPrintExpr body
prettyPrintExpr (LetCont aloc cont body) =
        concat [ [unwords ["letcont", prettyPrintTLoc aloc, "="]]
               , indent (prettyPrintCont cont)
               , prettyPrintExpr body
               ]
prettyPrintExpr (LetFunc aloc func body) =
        concat [ [unwords ["letfunc", prettyPrintTLoc aloc, "="]]
               , indent (prettyPrintFunc func)
               , prettyPrintExpr body
               ]
prettyPrintExpr (LetGlobalTuple label elements body) =
        unwords ["letglobaltuple", prettyPrintTLabel label, "=", prettyPrintElements elements]
        : prettyPrintExpr body
prettyPrintExpr (LetGlobalFunc label func body) =
        concat [ [unwords ["letglobalfunc", prettyPrintTLabel label, "="]]
               , indent (prettyPrintFunc func)
               , prettyPrintExpr body
               ]
prettyPrintExpr (LetGlobalFuncs labels funcs body) =
        concat [ ["letglobalfuncs"]
               , indent ( concat (zipWith (\label func -> unwords [prettyPrintTLabel label, "="] : indent (prettyPrintFunc func))
                                          labels
                                          funcs)
                        )
               , prettyPrintExpr body
               ]
prettyPrintExpr (If p c a) =
        concat [ [unwords ["if", prettyPrintPred p]]
               , indent (prettyPrintExpr c)
               , ["else"]
               , indent (prettyPrintExpr a)
               ]

prettyPrintPred :: Pred -> String
prettyPrintPred (RelOp op l r) = unwords [prettyPrintTLoc l, prettyPrintRelOp op, prettyPrintTLoc r]

prettyPrintRelOp :: RelOp -> String
prettyPrintRelOp Lt = "<"
prettyPrintRelOp Gt = ">"
prettyPrintRelOp Eq = "=="
prettyPrintRelOp Lte = "<="
prettyPrintRelOp Gte = ">="
prettyPrintRelOp Neq = "/="

prettyPrintElements :: [TAPlace] -> String
prettyPrintElements elements = "(" ++ intercalate ", " (map prettyPrintTPlace elements) ++ ")"

prettyPrintCont :: Cont -> [String]
prettyPrintCont (Cont arg body) =
        unwords ["cont", prettyPrintTLoc arg, "->"]
        : prettyPrintExpr body

prettyPrintFunc :: Func -> [String]
prettyPrintFunc (Func cont arg body) =
        unwords ["func", prettyPrintTLoc cont, prettyPrintTLoc arg, "->"]
        : prettyPrintExpr body

prettyPrintTPlace :: TAPlace -> String
prettyPrintTPlace (typ, place) = prettyPrintTagged typ (prettyPrintPlace place)

prettyPrintPlace :: APlace -> String
prettyPrintPlace (AAloc aloc) = prettyPrintLoc aloc
prettyPrintPlace (ALabel label) = prettyPrintLabel label

prettyPrintTLoc :: TAloc -> String
prettyPrintTLoc (typ, aloc) = prettyPrintTagged typ (prettyPrintLoc aloc)

prettyPrintLoc :: Aloc -> String
prettyPrintLoc (Aloc template n) = template ++ "$" ++ show n

prettyPrintTLabel :: TLabel -> String
prettyPrintTLabel (typ, label) = prettyPrintTagged typ (prettyPrintLabel label)

prettyPrintLabel :: Label -> String
prettyPrintLabel (Label template n) = template ++ "$" ++ show n
prettyPrintLabel HaltLabel = "halt"

prettyPrintValue :: Value -> String
prettyPrintValue (Int i) = show i
prettyPrintValue (Bool True) = "true"
prettyPrintValue (Bool False) = "false"
prettyPrintValue (VLabel label) = prettyPrintTLabel label
prettyPrintValue (TupleRef place index) = unwords ["#" ++ show index, prettyPrintTPlace place]
prettyPrintValue (NumOp op l r) = unwords [prettyPrintTLoc l, prettyPrintNumOp op, prettyPrintTLoc r]

prettyPrintNumOp :: NumOp -> String
prettyPrintNumOp Add = "+"
prettyPrintNumOp Sub = "-"
prettyPrintNumOp Mul = "*"

prettyPrintTagged :: Type -> String -> String
prettyPrintTagged typ s = unwords [s, ":", prettyPrintType typ]

prettyPrintType :: Type -> String
prettyPrintType (TFunc cont arg) = unwords [prettyPrintType' cont, prettyPrintType' arg, "->", "⊥"]
prettyPrintType (TCont arg) = unwords [prettyPrintType' arg, "->", "⊥"]
prettyPrintType typ = prettyPrintType' typ

prettyPrintType' :: Type -> String
prettyPrintType' TInt = "Int"
prettyPrintType' TBool = "Bool"
prettyPrintType' (TFunc cont arg) = parens (unwords [prettyPrintType' cont, prettyPrintType' arg, "->", "⊥"])
prettyPrintType' (TCont arg) = parens (unwords [prettyPrintType' arg, "->", "⊥"])
prettyPrintType' (TTuple elements) = parens (intercalate "," (map prettyPrintType' elements))

indent :: [String] -> [String]
indent lines = map ("    " ++) lines

parens :: String ->String
parens s = "(" ++ s ++ ")"
