module S64.Types where

import Compiler.Types

data Program = Program [GlobalTuple] [Stmt]
             deriving Show

data Stmt = Set Reg RTriv
          | NumOp NumOp Reg Reg
          | MRef Reg RPlace Int
          | MSet RPlace Int RPlace
          | Compare Reg Reg
          | JumpIf RelOp Label
          | Jump RPlace
          | Labelled Label Stmt
          deriving Show
