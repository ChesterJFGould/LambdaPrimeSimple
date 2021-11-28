module Pred.PrettyPrint
( prettyPrint
)
where

import Compiler.Types
import Pred.Types

import Data.List

prettyPrint :: Program -> String
prettyPrint (Program body) = unlines' (prettyPrintExpr body)

prettyPrintExpr :: TExpr -> [String]
prettyPrintExpr (typ, Atom atom) = [ prettyPrintAtom atom ]
prettyPrintExpr (typ, NumOp op l r) =
        annotateLines typ ( joins [ prettyPrintExpr l
                                  , [ concat [ " ", prettyPrintNumOp op, " " ] ]
                                  , prettyPrintExpr r
                                  ]
                          )
prettyPrintExpr (typ, Apply f arg) =
        annotateLines typ ( joins [ prettyPrintExpr f
                                  , [ " " ]
                                  , prettyPrintExpr arg
                                  ]
                          )
prettyPrintExpr (typ, Lambda aloc body) =
        let header = concat [ "λ ", prettyPrintAloc aloc, ". " ]
        in annotateLines typ ( joins [ [ header ]
                                     , indentTailBy (length header) (prettyPrintExpr body)
                                     ]
                             )
prettyPrintExpr (typ, Let aloc val body) =
        joins [ [ "let " ]
              , indentTailBy 4 (prettyPrintLet aloc val)
              , [ "", "in " ]
              , indentTailBy 3 (prettyPrintExpr body)
              ]
prettyPrintExpr (typ, LetRec alocs vals body) =
        joins [ [ "letrec " ]
              , indentTailBy 7 (concat (map (uncurry prettyPrintLet) (zip alocs vals)))
              , [ "", "in " ]
              , indentTailBy 3 (prettyPrintExpr body)
              ]
prettyPrintExpr (typ, If p c a) =
        annotateLines typ ( joins [ [ "if " ]
                                  , indentTailBy 3 (prettyPrintPred p)
                                  , [ "", "then " ]
                                  , indentTailBy 5 (prettyPrintExpr c)
                                  , [ "", "else " ]
                                  , indentTailBy 5 (prettyPrintExpr a)
                                  ]
                          )

prettyPrintPred :: Pred -> [String]
prettyPrintPred (RelOp op l r) =
        joins [ prettyPrintExpr l
              , [ concat [ " ", prettyPrintRelOp op, " " ] ]
              , prettyPrintExpr r
              ]

prettyPrintLet :: TAloc -> TExpr -> [String]
prettyPrintLet aloc val =
        let header = concat [ prettyPrintAloc aloc, " = " ]
        in joins [ [ header ]
                 , indentTailBy (length header) (prettyPrintExpr val)
                 ]

prettyPrintAtom :: TAtom -> String
prettyPrintAtom (typ, Int i) = annotate typ (show i)
prettyPrintAtom (typ, Bool b) = annotate typ (show b)
prettyPrintAtom (typ, AtAloc aloc) = prettyPrintAloc aloc

prettyPrintType :: Type -> String
prettyPrintType TInt = "Int"
prettyPrintType TBool = "Bool"
prettyPrintType (TFunc f@(TFunc _ _) t) = concat [ "(", prettyPrintType f, ") -> ", prettyPrintType t ]
prettyPrintType (TFunc f t) = concat [ prettyPrintType f, " -> ", prettyPrintType t ]

prettyPrintAloc :: TAloc -> String
prettyPrintAloc (typ, Aloc template n) = annotate typ (concat [ template , "_", show n ])

prettyPrintNumOp :: NumOp -> String
prettyPrintNumOp Add = "+"
prettyPrintNumOp Sub = "-"
prettyPrintNumOp Mul = "*"
prettyPrintNumOp Div = "/"
prettyPrintNumOp Mod = "%"

prettyPrintRelOp :: RelOp -> String
prettyPrintRelOp Lt = "<"
prettyPrintRelOp Gt = ">"
prettyPrintRelOp Eq = "=="
prettyPrintRelOp Lte = "<="
prettyPrintRelOp Gte = ">="
prettyPrintRelOp Neq = "/="

indentBy :: Int -> [String] -> [String]
indentBy n lines = map ((take n (repeat ' ')) ++) lines

indentTailBy :: Int -> [String] -> [String]
indentTailBy n lines = head lines : indentBy n (tail lines)

annotate :: Type -> String -> String
annotate typ s = concat [ "(", s, " : ", prettyPrintType typ, ")" ]

annotateLines :: Type -> [String] -> [String]
annotateLines typ [ line ] = [ annotate typ line ]
annotateLines typ lines =
        concat [ [ concat [ "( ", head lines ] ]
               , indentBy 2 (tail lines)
               , [ concat [ ": ", prettyPrintType typ ] ]
               , [")"]
               ]

unlines' :: [String] -> String
unlines' lines = intercalate "\n" lines

join :: [String] -> [String] -> [String]
join a b = init a ++ [ last a ++ head b ] ++ tail b

joins :: [[String]] -> [String]
joins list = foldl join (head list) (tail list)

maxLength :: [[String]] -> Int
maxLength list = maximum (map length list)
