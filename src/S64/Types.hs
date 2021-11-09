module S64.Types where

import Compiler.Types

data Program = Program [Stmt]

data Stmt = Set Reg RTriv
          | NumOp NumOp Reg Reg
          | MRef Reg Reg Int
          | MSet Reg Int RPlace
          | Compare Reg Reg
          | JumpIf RelOp Label
          | Jump RPlace
          | Labelled Label Stmt
