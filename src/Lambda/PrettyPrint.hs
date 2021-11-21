module Lambda.PrettyPrint
( prettyPrint
)
where

import Compiler.Types
import Lambda.Types

import Data.List

prettyPrint :: Program -> String
prettyPrint (Program body) = unlines' (prettyPrintExpr body)

prettyPrintExpr :: TExpr -> [String]
prettyPrintExpr (typ, Atom atom) = [ prettyPrintAtom atom ]
prettyPrintExpr (typ, BinOp op l r) =
        annotateLines typ ( joins [ prettyPrintExpr l
                                  , [ concat [ " ", prettyPrintOp op, " " ] ]
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
                                  , indentTailBy 3 (prettyPrintExpr p)
                                  , [ "", "then " ]
                                  , indentTailBy 5 (prettyPrintExpr c)
                                  , [ "", "else " ]
                                  , indentTailBy 5 (prettyPrintExpr a)
                                  ]
                          )

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

prettyPrintOp :: BinOp -> String
prettyPrintOp (Num Add) = "+"
prettyPrintOp (Num Sub) = "-"
prettyPrintOp (Num Mul) = "*"
prettyPrintOp (Num Div) = "/"
prettyPrintOp (Num Mod) = "%"
prettyPrintOp (Rel Lt) = "<"
prettyPrintOp (Rel Gt) = ">"
prettyPrintOp (Rel Eq) = "=="
prettyPrintOp (Rel Lte) = "<="
prettyPrintOp (Rel Gte) = ">="
prettyPrintOp (Rel Neq) = "/="

indentBy :: Int -> [String] -> [String]
indentBy n lines = map ((take n (repeat ' ')) ++) lines

indentTailBy :: Int -> [String] -> [String]
indentTailBy n lines = head lines : indentBy n (tail lines)

annotate :: Type -> String -> String
annotate typ s = concat [ "(", s, " : ", prettyPrintType typ, ")" ]

annotateLines :: Type -> [String] -> [String]
annotateLines typ [ line ] = [ annotate typ line ]
annotateLines typ lines =
        concat [ "(", head lines ]
        : indentBy 1 ( init (tail lines)
                       ++ [ concat [ last lines
                                   , " : "
                                   , prettyPrintType typ
                                   , ")"
                                   ]
                          ]
                     )

unlines' :: [String] -> String
unlines' lines = intercalate "\n" lines

join :: [String] -> [String] -> [String]
join a b = init a ++ [ last a ++ head b ] ++ tail b

joins :: [[String]] -> [String]
joins list = foldl join (head list) (tail list)

maxLength :: [[String]] -> Int
maxLength list = maximum (map length list)
