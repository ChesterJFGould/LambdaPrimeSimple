module Compiler.Gensym where

import Compiler.Types

import Control.Monad.State

type Gensym = State Int

evalGensym :: Gensym a -> a
evalGensym computation = evalState computation 0

genAloc :: String -> Gensym Aloc
genAloc template =
        do
        i <- get
        let aloc = Aloc template i
        put (i + 1)
        return aloc

genLabel :: String -> Gensym Label
genLabel template =
        do
        i <- get
        let label = Label template i
        put (i + 1)
        return label
