module Compiler.Types where

import Data.Word

data FileLocation = FileLocation { file :: String
                                 , line :: Int
                                 , column :: Int
                                 }

instance Show FileLocation where
         show (FileLocation _ line column) = show line ++ " : " ++ show column

data BinOp = Num NumOp
           | Rel RelOp
           deriving Show

data RelOp = Lt
           | Gt
           | Eq
           | Lte
           | Gte
           | Neq
           deriving Show

data NumOp = Add
           | Sub
           | Mul
           | Div
           | Mod
           deriving Show

data Var = Var String
         deriving (Eq, Ord, Show)

data Aloc = Aloc String Int

data Mloc = MAloc Aloc
          | MReg Reg

data Label = Label String Int

data APlace = AAloc Aloc
            | ALabel Label

data MPlace = MMloc Mloc
            | MLabel Label

data RPlace = RReg Reg
            | RLabel Label

data MTriv = MPlace MPlace
           | MWord Word64

data RTriv = RPlace RPlace
           | RWord Word64

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
