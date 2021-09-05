module CPSFree.Types where

import Compiler.Types

import qualified Data.Set.Ordered as O

type Free = O.OSet Mloc

type Tagged a = (Free, a)

data Program = Program [GlobalTuple] [Block] Body
             deriving Show

data Block = Block Label Body
           deriving Show

data Body = Body [Reg] TExpr
          deriving Show

type TExpr = Tagged Expr

data Expr = Set Mloc MTriv TExpr
          | NumOp NumOp Mloc Mloc TExpr
          | MRef Mloc MPlace Int TExpr
          | MSet MPlace Int MPlace TExpr
          | If Pred TExpr TExpr
          | Jump MPlace [Reg]
          deriving Show

data Pred = RelOp RelOp Mloc Mloc
          deriving Show
