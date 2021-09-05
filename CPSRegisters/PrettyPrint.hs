module CPSRegisters.PrettyPrint
( prettyPrint
)
where

import Compiler.Types
import CPSRegisters.Types

import Data.List
import Data.Word

prettyPrint :: Program -> String
prettyPrint (Program tuples blocks body) =
        intercalate "\n" ( intercalate ["\n"] [ prettyPrintTuples tuples
                                                , prettyPrintBlocks blocks
                                                , prettyPrintBody body
                                                ]
                         )

prettyPrintTuples :: [GlobalTuple] -> [String]
prettyPrintTuples tuples = intersperse "\n" (map prettyPrintTuple tuples)

prettyPrintTuple :: GlobalTuple -> String
prettyPrintTuple (GlobalTuple label elements) =
        concat [ prettyPrintLabel label
               , ": ("
               , intercalate ", " (map prettyPrintTupleTriv elements)
               , ")"
               ]

prettyPrintTupleTriv :: TupleTriv -> String
prettyPrintTupleTriv (TLabel label) = prettyPrintLabel label
prettyPrintTupleTriv (TWord word) = prettyPrintWord word

prettyPrintBlocks :: [Block] -> [String]
prettyPrintBlocks blocks = intercalate ["\n"] (map prettyPrintBlock blocks)

prettyPrintBlock :: Block -> [String]
prettyPrintBlock (Block label body) =
        (prettyPrintLabel label ++ ":")
        : indent (prettyPrintBody body)

prettyPrintBody :: Body -> [String]
prettyPrintBody (Body expr) = prettyPrintExpr expr

prettyPrintExpr :: Expr -> [String]
prettyPrintExpr (Set reg triv body) =
        unwords [prettyPrintReg reg, ":=", prettyPrintTriv triv]
        : prettyPrintExpr body
prettyPrintExpr (NumOp op l r body) =
        unwords [prettyPrintReg l, prettyPrintNumOp op, prettyPrintReg r]
        : prettyPrintExpr body
prettyPrintExpr (MRef reg ptr offset body) =
        unwords [prettyPrintReg reg, ":=", prettyPrintMRef ptr offset]
        : prettyPrintExpr body
prettyPrintExpr (MSet ptr offset val body) =
        unwords [prettyPrintMRef ptr offset, ":=", prettyPrintPlace val]
        : prettyPrintExpr body
prettyPrintExpr (If p c a) =
        concat [ [unwords ["if", prettyPrintPred p]]
               , indent (prettyPrintExpr c)
               , ["else"]
               , indent (prettyPrintExpr a)
               ]
prettyPrintExpr (Jump place) = [unwords ["jump", prettyPrintPlace place]]

prettyPrintPred :: Pred -> String
prettyPrintPred (RelOp op l r) = unwords [prettyPrintReg l, prettyPrintRelOp op, prettyPrintReg r]

prettyPrintRelOp :: RelOp -> String
prettyPrintRelOp Lt = "<"
prettyPrintRelOp Gt = ">"
prettyPrintRelOp Eq = "=="
prettyPrintRelOp Lte = "<="
prettyPrintRelOp Gte = ">="
prettyPrintRelOp Neq = "/="

prettyPrintLabel :: Label -> String
prettyPrintLabel (Label template n) = template ++ "$" ++ show n
prettyPrintLabel HaltLabel = "halt"

prettyPrintPlace :: RPlace -> String
prettyPrintPlace (RReg reg) = prettyPrintReg reg
prettyPrintPlace (RLabel label) = prettyPrintLabel label

prettyPrintTriv :: RTriv -> String
prettyPrintTriv (RPlace place) = prettyPrintPlace place
prettyPrintTriv (RWord word) = prettyPrintWord word

prettyPrintWord :: Word64 -> String
prettyPrintWord = show

prettyPrintReg :: Reg -> String
prettyPrintReg = show

prettyPrintNumOp :: NumOp -> String
prettyPrintNumOp Add = "+="
prettyPrintNumOp Sub = "-="
prettyPrintNumOp Mul = "*="

prettyPrintMRef :: RPlace -> Int -> String
prettyPrintMRef ptr offset = unwords ["[", prettyPrintPlace ptr, "+", show offset, "]"]

indent :: [String] -> [String]
indent lines = map ("    " ++) lines
