module Main where

import Compiler.Gensym

import AST
import CPS
import Lambda
import Pred
import TAST
import TAST.Types

import System.Exit
import System.IO
import Text.Pretty.Simple

compile :: Program -> IO ()
compile program =
        do
        ( putStrLn
          . CPS.prettyPrint
          . evalGensym
          . (>>= Pred.lower)
          . fmap Lambda.lower
          . TAST.lower
          ) program

handleError :: String -> IO ()
handleError msg =
        do
        hPutStrLn stderr msg
        exitFailure

main :: IO ()
main =
        do
        getContents >>= either handleError compile
                        . (>>= AST.check)
                        . AST.parse "stdin"
