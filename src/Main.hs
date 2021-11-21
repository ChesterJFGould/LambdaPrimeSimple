module Main where

import Compiler.Gensym

import AST
import Lambda
import TAST
import TAST.Types

import System.Exit
import System.IO
import Text.Pretty.Simple

compile :: Program -> IO ()
compile program =
        do
        ( putStrLn
          . Lambda.prettyPrint
          . evalGensym
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
