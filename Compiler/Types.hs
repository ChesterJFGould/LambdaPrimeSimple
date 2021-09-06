module Compiler.Types where

import Data.Word

data BinOp = BNumOp NumOp
           | BRelOp RelOp
           deriving Show

data NumOp = Add
           | Sub
           | Mul
           | Div
           | Mod
           deriving Show

data RelOp = Lt
           | Gt
           | Eq
           | Lte
           | Gte
           | Neq
           deriving Show

data Aloc = Aloc String Int
          deriving (Eq, Ord, Show)

data Mloc = MAloc Aloc
          | MReg Reg
          deriving (Eq, Ord, Show)

data Label = Label String Int
           | HaltLabel
           deriving (Eq, Ord, Show)

data APlace = AAloc Aloc
            | ALabel Label
            deriving Show

data MPlace = MMloc Mloc
            | MLabel Label
            deriving Show

data RPlace = RReg Reg
            | RLabel Label
            deriving Show

data MTriv = MPlace MPlace
           | MWord Word64
           deriving Show

data RTriv = RPlace RPlace
           | RWord Word64
           deriving Show

data GlobalTuple = GlobalTuple Label [TupleTriv]
                 deriving Show

data TupleTriv = TLabel Label
               | TWord Word64
               deriving Show

data Reg = RSP
         | RBP
         | RAX
         | RBX
         | RCX
         | RDX
         | RSI
         | RDI
         | R8
         | R9
         | R10
         | R11
         | R12
         | R13
         | R14
         | R15
         deriving (Eq, Ord, Show)
