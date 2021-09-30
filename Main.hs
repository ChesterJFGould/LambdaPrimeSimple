module Main where

import qualified AST
import qualified CPS
import qualified CPSAsm
import qualified CPSClosures
import qualified CPSFree
import qualified CPSRegisters
import qualified Globals
import qualified Lambda
import Compiler.Gensym
import qualified Pred
import qualified S64
import qualified TAST
import TAST.Types

import System.Exit
import System.IO
import Text.Pretty.Simple

compile :: Program -> IO ()
compile program =
        do
        prelude <- readFile "Runtime/prelude.s"
        postlude <- readFile "Runtime/postlude.s"
        ( putStrLn
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
          . TAST.lower
          ) program

prettyPrintCPS :: Program -> IO ()
prettyPrintCPS program =
        ( putStrLn
          . evalGensym
          . fmap CPS.prettyPrint
          . (>>= Globals.lower)
          . (>>= Pred.lower)
          . fmap Lambda.lower
          . TAST.lower
          ) program

prettyPrintCPSClosures :: Program -> IO ()
prettyPrintCPSClosures program =
        ( putStrLn
          . evalGensym
          . fmap CPSClosures.prettyPrint
          . (>>= CPS.lower)
          . (>>= Globals.lower)
          . (>>= Pred.lower)
          . fmap Lambda.lower
          . TAST.lower
          ) program

prettyPrintCPSRegisters :: Program -> IO ()
prettyPrintCPSRegisters program =
        ( putStrLn
          . evalGensym
          . fmap CPSRegisters.prettyPrint
          . fmap CPSFree.lower
          . fmap CPSAsm.lower
          . (>>= CPSClosures.lower)
          . (>>= CPS.lower)
          . (>>= Globals.lower)
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
main = do
       getContents >>= either handleError compile
                       . (>>= AST.check)
                       . AST.parse "stdin"
