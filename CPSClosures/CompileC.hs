module CPSClosures.CompileC where

import Compiler.Types
import CPSClosures.Types

import Data.List

compileC :: String -> String -> Program -> String
compileC prelude init program@(Program defs body) =
        prelude ++ intercalate "\n" ( concat [ map cDecl (globalTuples program ++ map defLabel defs)
                                             , [""]
                                             , intercalate [""] (map cDef defs)
                                             , [""]
                                             , cMain init body
                                             ]
                                    )

cMain :: String -> Body -> [String]
cMain init body =
        [ "void"
        , "main()"
        , "{"
        , init
        ]
        ++
        indent (cBody body)
        ++
        ["}"]

cDecl :: TLabel -> String
cDecl (TInt, label) = concat [ cType TInt, " ", cLabel label, ";" ]
cDecl (TBool, label) = concat [ cType TBool, " ", cLabel label, ";" ]
cDecl (TFunc cont arg, label) =
        concat [ "void ", cLabel label, "(uint64_t*, ", cType cont, ", ", cType arg, ");" ]
cDecl (TCont arg, label) =
        concat [ "void ", cLabel label, "(uint64_t*, ", cType arg, ");" ]
cDecl (TTuple elements, label) = concat [ cType (TTuple elements), " ", cLabel label, ";" ]
cDecl (TClosure typ, label) = concat [ cType (TClosure typ), " ", cLabel label, ";" ]

cDef :: Def -> [String]
cDef (Func tLabel env cont arg body) =
        [ "void"
        , concat [ cTLabel tLabel, "(", intercalate ", " (map cTLocDecl [env, cont, arg]), ")" ]
        , "{"
        ]
        ++
        indent (cBody body)
        ++
        ["}"]
cDef (Cont tLabel env arg body) =
        [ "void"
        , concat [ cTLabel tLabel, "(", intercalate ", " (map cTLocDecl [env, arg]), ")" ]
        , "{"
        ]
        ++
        indent (cBody body)
        ++
        ["}"]

defLabel :: Def -> TLabel
defLabel (Func label _ _ _ _) = label
defLabel (Cont label _ _ _) = label

cBody :: Body -> [String]
cBody (Body expr) = cExpr expr

cExpr :: Expr -> [String]
cExpr (CallFunc f env cont arg) =
        [ concat [ "return", " ", cTPlace f, "(", intercalate "," (map cTPlace [env, cont, arg]), ")", ";" ] ]
cExpr (CallCont cont env arg) =
        [ concat [ "return", " ", cTPlace cont, "(", cTPlace env, ", ", cTPlace arg, ")", ";" ] ]
cExpr (Let loc val body) =
        cAssignment loc val ++ cExpr body
cExpr (LetGlobalTuple label elements body) =
        cGlobalTupleInit label elements ++ cExpr body
cExpr (If p c a) =
        concat [ [ concat [ "if (", cPred p, ") {" ] ]
               , indent (cExpr c)
               , [ "} else {" ]
               , indent (cExpr a)
               , [ "}" ]
               ]

cPred :: Pred -> String
cPred (RelOp op l r) = unwords [ cTLoc l, cRelOp op, cTLoc r ]

cAssignment :: TAloc -> Value -> [String]
cAssignment tLoc (Int i) = [ concat [ cTLocDecl tLoc, " = ", show i, ";" ] ]
cAssignment tLoc (Bool True) = [ concat [ cTLocDecl tLoc, " = 1;" ] ]
cAssignment tLoc (Bool False) = [ concat [ cTLocDecl tLoc, " = 0;" ] ]
cAssignment tLoc (VLabel tLabel) = [ concat [ cTLocDecl tLoc, " = ", cTLabel tLabel, ";"] ]
cAssignment tLoc (Tuple elements) =
        concat [ cTLocDecl tLoc, " = alloca(sizeof(uint64_t) * ", show (length elements), ");" ]
        : initElements (cTLoc tLoc) elements
cAssignment tLoc (TupleRef tPlace index) =
        [ concat [ cTLocDecl tLoc, " = ", cTPlace tPlace, "[", show index, "];" ] ]
cAssignment tLoc (NumOp op l r) = [ concat [ cTLocDecl tLoc, " = ", cTLoc l, " ", cNumOp op, cTLoc r, ";" ] ]

initElements :: String -> [TAPlace] -> [String]
initElements tuple elements = initElements' tuple (zip [0..] elements)

initElements' :: String -> [(Int, TAPlace)] -> [String]
initElements' _ [] = []
initElements' tuple ((i, tPlace) : rest) =
        concat [ tuple, "[", show i, "] = ", cTPlace tPlace, ";" ]
        : initElements' tuple rest

cGlobalTupleInit :: TLabel -> [TAPlace] -> [String]
cGlobalTupleInit tLabel elements =
        concat [ cTLabel tLabel, " = alloca(sizeof(uint64_t) * ", show (length elements), ");" ]
        : initElements (cTLabel tLabel) elements

cTPlace :: TAPlace -> String
cTPlace (_, place) = cPlace place

cPlace :: APlace -> String
cPlace (AAloc loc) = cLoc loc
cPlace (ALabel label) = cLabel label

cTLocDecl :: TAloc -> String
cTLocDecl (typ, loc) = cTPlaceDecl (typ, AAloc loc)

cTLabelDecl :: TLabel -> String
cTLabelDecl (typ, label) = cTPlaceDecl (typ, ALabel label)

cTPlaceDecl :: TAPlace -> String
cTPlaceDecl (TInt, place) = unwords [ cType TInt, cPlace place ]
cTPlaceDecl (TBool, place) = unwords [ cType TBool, cPlace place ]
cTPlaceDecl (TFunc cont arg, place) = concat [ "void (*", cPlace place, ")(uint64_t*, ", cType cont, ", ", cType arg, ")" ]
cTPlaceDecl (TCont arg, place) = concat [ "void (*", cPlace place, ")(uint64_t*, ", cType arg, ")" ]
cTPlaceDecl (TTuple elements, place) = unwords [ cType (TTuple elements), cPlace place ]
cTPlaceDecl (TClosure typ, place) = unwords [ cType (TClosure typ), cPlace place ]

cTLoc :: TAloc -> String
cTLoc (_, loc) = cLoc loc

cLoc :: Aloc -> String
cLoc (Aloc template n) = concat [ template, "_", show n ]

cTLabel :: TLabel -> String
cTLabel (_, label) = cLabel label

cLabel :: Label -> String
cLabel (Label template n) = concat [ template, "_", show n ]
cLabel HaltLabel = "halt"

cType :: Type -> String
cType TInt = "int64_t"
cType TBool = "uint64_t"
cType (TFunc cont arg) = concat [ "void (uint64_t*, ", cType cont, ", ", cType arg, ")" ]
cType (TCont arg) = concat [ "void (uint64_t*, ", cType arg, ")" ]
cType (TTuple _) = "uint64_t*"
cType (TClosure _) = "uint64_t*"

cRelOp :: RelOp -> String
cRelOp Lt = "<"
cRelOp Gt = ">"
cRelOp Eq = "=="
cRelOp Lte = "<="
cRelOp Gte = ">="
cRelOp Neq = "!="

cNumOp :: NumOp -> String
cNumOp Add = "+"
cNumOp Sub = "-"
cNumOp Mul = "*"
cNumOp Div = "/"
cNumOp Mod = "%"

globalTuples :: Program -> [TLabel]
globalTuples (Program defs body) = nub (concat (map gtDef defs) ++ gtBody body)

gtDef :: Def -> [TLabel]
gtDef (Func _ _ _ _ body) = gtBody body
gtDef (Cont _ _ _ body) = gtBody body

gtBody :: Body -> [TLabel]
gtBody (Body expr) = gtExpr expr

gtExpr :: Expr -> [TLabel]
gtExpr (CallFunc _ _ _ _) = []
gtExpr (CallCont _ _ _) = []
gtExpr (Let _ _ body) = gtExpr body
gtExpr (LetGlobalTuple label _ body) = label : gtExpr body
gtExpr (If _ c a) = gtExpr c ++ gtExpr a

indent :: [String] -> [String]
indent lines = map ('\t' :) lines
