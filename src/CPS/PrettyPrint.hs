module CPS.PrettyPrint
( prettyPrint
)
where

import Compiler.Types
import CPS.Types

import Data.List

prettyPrint :: Program -> String
prettyPrint (Program expr) = unlines' (ppExpr expr)

ppExpr :: Expr -> [String]
ppExpr (CallFunc f cont arg) =
        [ unwords (map ppAloc [f, cont, arg]) ]
ppExpr (CallCont cont arg) =
        [ unwords (map ppAloc [cont, arg]) ]
ppExpr (Let aloc atom body) =
        unwords [ "let", ppAloc aloc, "=", ppAtom atom ]
        : ppExpr body
ppExpr (LetCont aloc cont body) =
        concat [ join [ "letcont " ] (indentTailBy 8 (ppCont aloc cont))
               , ppExpr body
               ]
ppExpr (LetRecFuncs alocs funcs body) =
        concat [ join [ "letfuncs " ] (indentTailBy 9 (concat (zipWith ppFunc alocs funcs)))
               , ppExpr body
               ]
ppExpr (If p c a) =
        concat [ [ unwords [ "if", ppPred p ] ]
               , join [ "then " ] (indentTailBy 5 (ppExpr c))
               , join [ "else " ] (indentTailBy 5 (ppExpr a))
               ]

ppPred :: Pred -> String
ppPred (RelOp op l r) = unwords [ ppAloc l, ppRelOp op, ppAloc r ]

ppCont :: TAloc -> Cont -> [String]
ppCont name (Cont arg body) =
        unwords [ ppAloc name, ppAloc arg, "=" ]
        : indentBy 4 (ppExpr body)

ppFunc :: TAloc -> Func -> [String]
ppFunc name (Func cont arg body) =
        unwords [ ppAloc name, ppAloc cont, ppAloc arg, "=" ]
        : indentBy 4 (ppExpr body)

ppAtom :: Atom -> String
ppAtom (Int i) = show i
ppAtom (Bool b) = show b
ppAtom (AtLabel label) = ppLabel label
ppAtom (NumOp op l r) = unwords [ ppAloc l, ppNumOp op, ppAloc r ]

ppLabel :: TLabel -> String
ppLabel (typ, label) = annotate (ppLabel' label) typ

ppLabel' :: Label -> String
ppLabel' (Label template n) = concat [ template, "_", show n ]
ppLabel' HaltLabel = "halt"

ppAloc :: TAloc -> String
ppAloc (typ, aloc) = annotate (ppAloc' aloc) typ

ppAloc' :: Aloc -> String
ppAloc' (Aloc template n) = concat [ template, "_", show n ]

ppRelOp :: RelOp -> String
ppRelOp Lt = "<"
ppRelOp Gt = ">"
ppRelOp Eq = "=="
ppRelOp Lte = "<="
ppRelOp Gte = ">="
ppRelOp Neq = "/="

ppNumOp :: NumOp -> String
ppNumOp Add = "+"
ppNumOp Sub = "-"
ppNumOp Mul = "*"
ppNumOp Div = "/"
ppNumOp Mod = "%"

ppType :: Type -> String
ppType TInt = "Int"
ppType TBool = "Bool"
ppType (TFunc cont arg) = unwords [ "Func", ppType' cont, ppType' arg ]
ppType (TCont arg) = unwords [ "Cont", ppType' arg ]

ppType' :: Type -> String
ppType' typ@(TFunc _ _) = parens (ppType typ)
ppType' typ@(TCont _) = parens (ppType typ)
ppType' typ = ppType typ

unlines' :: [String] -> String
unlines' lines = intercalate "\n" lines

join :: [String] -> [String] -> [String]
join l r = init l ++ [last l ++ head r] ++ tail r

indentBy :: Int -> [String] -> [String]
indentBy n lines = map (take n (repeat ' ') ++) lines

indentTailBy :: Int -> [String] -> [String]
indentTailBy n lines = head lines : indentBy n (tail lines)

annotate :: String -> Type -> String
annotate s typ = parens (unwords [ s, ":", ppType typ ])

parens :: String -> String
parens s = concat [ "(", s, ")" ]
