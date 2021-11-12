module Main where

import AST

import System.Exit
import System.IO
import Text.Pretty.Simple

handleError :: String -> IO ()
handleError msg =
        do
        hPutStrLn stderr msg
        exitFailure

main :: IO ()
main =
        do
        getContents >>= either handleError pPrintForceColor
                        . (>>= AST.check)
                        . AST.parse "stdin"
