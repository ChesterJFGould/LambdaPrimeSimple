module Main where

import AST
import CPS
import CPSAsm
import CPSClosures
import CPSFree
import CPSRegisters
import Globals
import Lambda
import Compiler.Gensym
import Pred
import S64

import Text.Pretty.Simple

main :: IO ()
main = do
       prelude <- readFile "Runtime/prelude.s"
       postlude <- readFile "Runtime/postlude.s"
       getContents >>= either putStrLn putStrLn
                       . (>>= return
                              . evalGensym
                              . fmap (S64.compile prelude postlude)
                              . (>>= CPSRegisters.lower)
                              . fmap CPSFree.lower
                              . fmap CPSAsm.lower
                              . (>>= CPSClosures.lower)
                              . (>>= CPS.lower)
                              . (>>= Globals.lower)
                              . (>>= Pred.lower)
                              . fmap Lambda.lower
                              . AST.lower
                         )
                       . (>>= AST.check)
                       . AST.parse "stdin"
