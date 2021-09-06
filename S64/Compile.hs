module S64.Compile
( compile
)
where

import Compiler.Types
import S64.Types

import Data.List
import Data.Word

compile :: String -> String -> Program -> String
compile prelude postlude (Program tuples stmts) =
        intercalate "\n" [ prelude
                         , compileGlobalTuples tuples
                         , compileStmts stmts
                         , postlude
                         ]

compileGlobalTuples :: [GlobalTuple] -> String
compileGlobalTuples tuples =
        intercalate "\n" [ "section .data"
                         , intercalate "\n" (map compileGlobalTuple tuples)
                         , "section .text"
                         ]

compileGlobalTuple :: GlobalTuple -> String
compileGlobalTuple (GlobalTuple label elements) =
        concat [ compileLabel label
               , ": dq "
               , intercalate ", " (map compileTupleTriv elements)
               ]

compileTupleTriv :: TupleTriv -> String
compileTupleTriv (TLabel label) = compileLabel label
compileTupleTriv (TWord word) = compileWord word

compileStmts :: [Stmt] -> String
compileStmts stmts = intercalate "\n" (map compileStmt stmts)

compileStmt :: Stmt -> String
compileStmt (Set reg triv) = unwords [ "mov", compileReg reg, ",", compileTriv triv ]
compileStmt (NumOp Div l r) = intercalate "\n" [ unwords ["mov", compileReg RAX, ",", compileReg l]
                                               , "cqo"
                                               , unwords ["idiv", compileReg r]
                                               , unwords ["mov", compileReg l, ",", compileReg RAX]
                                               ]
compileStmt (NumOp Mod l r) = intercalate "\n" [ unwords ["mov", compileReg RAX, ",", compileReg l]
                                               , "cqo"
                                               , unwords ["idiv", compileReg r]
                                               , unwords ["mov", compileReg l, ",", compileReg RDX]
                                               ]
compileStmt (NumOp op l r) = unwords [ compileNumOp op, compileReg l, ",", compileReg r ]
compileStmt (MRef reg ptr offset) = unwords [ "mov", compileReg reg, ",", compileMRef ptr offset ]
compileStmt (MSet ptr offset val) = unwords [ "mov", compileMRef ptr offset, ",", compilePlace val ]
compileStmt (Compare l r) = unwords [ "cmp", compileReg l, ",", compileReg r ]
compileStmt (JumpIf op label) = unwords [ compileRelOp op, compileLabel label ]
compileStmt (Jump place) = unwords [ "jmp", compilePlace place ]
compileStmt (Labelled label stmt) = concat [ compileLabel label, ":\n", compileStmt stmt ]

compileMRef :: RPlace -> Int -> String
compileMRef ptr offset = unwords [ "QWORD", "[", compilePlace ptr, "+", show offset, "]" ]

compileTriv :: RTriv -> String
compileTriv (RPlace place) = compilePlace place
compileTriv (RWord word) = compileWord word

compilePlace :: RPlace -> String
compilePlace (RReg reg) = compileReg reg
compilePlace (RLabel label) = compileLabel label

compileNumOp :: NumOp -> String
compileNumOp Add = "add"
compileNumOp Sub = "sub"
compileNumOp Mul = "imul"
compileNumOp Div = error "Code generation for division must be handled seperately"

compileRelOp :: RelOp -> String
compileRelOp Lt = "jl"
compileRelOp Gt = "jg"
compileRelOp Eq = "je"
compileRelOp Lte = "jle"
compileRelOp Gte = "jge"
compileRelOp Neq = "jne"

compileReg :: Reg -> String
compileReg = show

compileLabel :: Label -> String
compileLabel (Label template n) = template ++ "$" ++ show n
compileLabel HaltLabel = "halt"

compileWord :: Word64 -> String
compileWord = show
