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
        [unwords [prettyPrintPlace f, prettyPrintPlace cont, prettyPrintPlace arg]]
prettyPrintExpr (CallCont cont arg) =
        [unwords [prettyPrintPlace cont, prettyPrintPlace arg]]
prettyPrintExpr (Let aloc val body) =
        unwords ["let", prettyPrintLoc aloc, "=", prettyPrintValue val]
        : prettyPrintExpr body
prettyPrintExpr (LetCont aloc cont body) =
        concat [ [unwords ["letcont", prettyPrintLoc aloc, "="]]
               , indent (prettyPrintCont cont)
               , prettyPrintExpr body
               ]
prettyPrintExpr (LetFunc aloc func body) =
        concat [ [unwords ["letfunc", prettyPrintLoc aloc, "="]]
               , indent (prettyPrintFunc func)
               , prettyPrintExpr body
               ]
prettyPrintExpr (LetGlobalTuple label elements body) =
        unwords ["letglobaltuple", prettyPrintLabel label, "=", prettyPrintElements elements]
        : prettyPrintExpr body
prettyPrintExpr (LetGlobalFunc label func body) =
        concat [ [unwords ["letglobalfunc", prettyPrintLabel label, "="]]
               , indent (prettyPrintFunc func)
               , prettyPrintExpr body
               ]
prettyPrintExpr (LetGlobalFuncs labels funcs body) =
        concat [ ["letglobalfuncs"]
               , indent ( concat (zipWith (\label func -> unwords [prettyPrintLabel label, "="] : indent (prettyPrintFunc func))
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
prettyPrintPred (RelOp op l r) = unwords [prettyPrintLoc l, prettyPrintRelOp op, prettyPrintLoc r]

prettyPrintRelOp :: RelOp -> String
prettyPrintRelOp Lt = "<"
prettyPrintRelOp Gt = ">"
prettyPrintRelOp Eq = "=="
prettyPrintRelOp Lte = "<="
prettyPrintRelOp Gte = ">="
prettyPrintRelOp Neq = "/="

prettyPrintElements :: [APlace] -> String
prettyPrintElements elements = "(" ++ intercalate ", " (map prettyPrintPlace elements) ++ ")"

prettyPrintCont :: Cont -> [String]
prettyPrintCont (Cont arg body) =
        unwords ["cont", prettyPrintLoc arg, "->"]
        : prettyPrintExpr body

prettyPrintFunc :: Func -> [String]
prettyPrintFunc (Func cont arg body) =
        unwords ["func", prettyPrintLoc cont, prettyPrintLoc arg, "->"]
        : prettyPrintExpr body

prettyPrintPlace :: APlace -> String
prettyPrintPlace (AAloc aloc) = prettyPrintLoc aloc
prettyPrintPlace (ALabel label) = prettyPrintLabel label

prettyPrintLoc :: Aloc -> String
prettyPrintLoc (Aloc template n) = template ++ "$" ++ show n

prettyPrintLabel :: Label -> String
prettyPrintLabel (Label template n) = template ++ "$" ++ show n
prettyPrintLabel HaltLabel = "halt"

prettyPrintValue :: Value -> String
prettyPrintValue (Int i) = show i
prettyPrintValue (Bool True) = "true"
prettyPrintValue (Bool False) = "false"
prettyPrintValue (VLabel label) = prettyPrintLabel label
prettyPrintValue (TupleRef place index) = unwords ["#" ++ show index, prettyPrintPlace place]
prettyPrintValue (NumOp op l r) = unwords [prettyPrintLoc l, prettyPrintNumOp op, prettyPrintLoc r]

prettyPrintNumOp :: NumOp -> String
prettyPrintNumOp Add = "+"
prettyPrintNumOp Sub = "-"
prettyPrintNumOp Mul = "*"

indent :: [String] -> [String]
indent lines = map ("    " ++) lines
