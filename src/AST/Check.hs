module AST.Check
( check
)
where

import AST.Types
import AST.TypeCheck
import Compiler.Types
import qualified TAST.Types as T

import Data.List

check :: Program -> Either String T.Program
check prog =
        do
        let typeErrors = typeCheck prog
        either (Left . formatErrors)
               Right
               typeErrors

formatErrors :: [CheckError] -> String
formatErrors errors = intercalate "\n" (map formatError errors)

formatError :: CheckError -> String
formatError (WithPos (FileLocation file line column) msg) =
        unwords [ "Error in file"
                , quote file
                , "at"
                , show line ++ ":" ++ show column
                , ":"
                , msg ++ "."
                ]
formatError (WithoutPos msg) =
        unwords [ "Error:"
                , msg ++ "."
                ]

quote :: String -> String
quote s = "\"" ++ s ++ "\""
