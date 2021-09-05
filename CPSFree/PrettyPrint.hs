module CPSFree.PrettyPrint
( prettyPrint
)
where

import Compiler.Types
import CPSFree.Types

import qualified Data.Foldable as F
import Data.List
import qualified Data.Set.Ordered as O
import Data.Word

prettyPrint :: Program -> String
prettyPrint (Program tuples blocks body) =
        intercalate "\n" ( concat [ prettyPrintTuples tuples
                                  , ["\n"]
                                  , prettyPrintBlocks blocks
                                  , ["\n"]
                                  , prettyPrintBody body
                                  ]
                         )

prettyPrintTuples :: [GlobalTuple] -> [String]
prettyPrintTuples tuples = map prettyPrintTuple tuples

prettyPrintTuple :: GlobalTuple -> String
prettyPrintTuple (GlobalTuple label tupleTrivs) =
        concat [ prettyPrintLabel label
               , ": ("
               , intercalate ", " (map prettyPrintTupleTriv tupleTrivs)
               , ")"
               ]

prettyPrintTupleTriv :: TupleTriv -> String
prettyPrintTupleTriv (TLabel label) = prettyPrintLabel label
prettyPrintTupleTriv (TWord word) = prettyPrintWord word

prettyPrintBlocks :: [Block] -> [String]
prettyPrintBlocks blocks = intercalate ["\n"] (map prettyPrintBlock blocks)

prettyPrintBlock :: Block -> [String]
prettyPrintBlock (Block label body) =
        (prettyPrintLabel label ++ ":") : indent (prettyPrintBody body)

prettyPrintBody :: Body -> [String]
prettyPrintBody (Body initialized expr) =
        prettyPrintRegs initialized : prettyPrintExpr expr

prettyPrintExpr :: TExpr -> [String]
prettyPrintExpr (freeIn, Set mloc triv body) =
        unwords [prettyPrintLoc mloc, ":=", prettyPrintTriv triv, "|", prettyPrintFree freeIn]
        : prettyPrintExpr body
prettyPrintExpr (freeIn, NumOp op l r body) =
        unwords [prettyPrintLoc l, prettyPrintNumOp op, prettyPrintLoc r, "|", prettyPrintFree freeIn]
        : prettyPrintExpr body
prettyPrintExpr (freeIn, MRef loc ptr offset body) =
        unwords [prettyPrintLoc loc, ":=", prettyPrintMRef ptr offset, "|", prettyPrintFree freeIn]
        : prettyPrintExpr body
prettyPrintExpr (freeIn, MSet ptr offset place body) =
        unwords [prettyPrintMRef ptr offset, ":=", prettyPrintPlace place, "|", prettyPrintFree freeIn]
        : prettyPrintExpr body
prettyPrintExpr (freeIn, If p c a) =
        concat [ [unwords ["if", prettyPrintPred p, "|", prettyPrintFree freeIn]]
               , indent (prettyPrintExpr c)
               , ["else"]
               , indent (prettyPrintExpr a)
               ]
prettyPrintExpr (freeIn, Jump place used) =
        [unwords ["jump", prettyPrintPlace place, prettyPrintRegs used, "|", prettyPrintFree freeIn]]

prettyPrintPred :: Pred -> String
prettyPrintPred (RelOp op l r) = unwords [prettyPrintLoc l, prettyPrintRelOp op, prettyPrintLoc r]

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

prettyPrintFree :: O.OSet Mloc -> String
prettyPrintFree free = list (map prettyPrintLoc (F.toList free))

prettyPrintRegs :: [Reg] -> String
prettyPrintRegs regs = list (map prettyPrintReg regs)

prettyPrintReg :: Reg -> String
prettyPrintReg = show

prettyPrintLoc :: Mloc -> String
prettyPrintLoc (MAloc aloc) = prettyPrintAloc aloc
prettyPrintLoc (MReg reg) = prettyPrintReg reg

prettyPrintAloc :: Aloc -> String
prettyPrintAloc (Aloc template n) = template ++ "$" ++ show n

prettyPrintTriv :: MTriv -> String
prettyPrintTriv (MPlace place) = prettyPrintPlace place
prettyPrintTriv (MWord word) = prettyPrintWord word

prettyPrintPlace :: MPlace -> String
prettyPrintPlace (MMloc loc) = prettyPrintLoc loc
prettyPrintPlace (MLabel label) = prettyPrintLabel label

prettyPrintNumOp :: NumOp -> String
prettyPrintNumOp Add = "+="
prettyPrintNumOp Sub = "-="
prettyPrintNumOp Mul = "*="

prettyPrintMRef :: MPlace -> Int -> String
prettyPrintMRef ptr offset = unwords ["[", prettyPrintPlace ptr, "+", show offset, "]"]

prettyPrintWord :: Word64 -> String
prettyPrintWord = show

indent :: [String] -> [String]
indent lines = map ("    " ++) lines

list :: [String] -> String
list elements = "[" ++ intercalate ", " elements ++ "]"
