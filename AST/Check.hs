module AST.Check
( check
)
where

import AST.DefCheck
import AST.TypeCheck
import AST.Types

import Data.List

check :: Program -> Either String Program
check prog = do
             let defErrors = defCheck prog
             case defErrors of
                  [] -> Right ()
                  _ -> Left (formatErrors defErrors)
             let typeErrors = typeCheck prog
             case typeErrors of
                  [] -> Right ()
                  _ -> Left (formatErrors typeErrors)
             Right prog

formatErrors :: [CheckError] -> String
formatErrors errors = intercalate "\n\n" (map formatError errors)

formatError :: CheckError -> String
formatError (WithPos pos err) = unwords [ "Error at"
                                        , show pos
                                        , ":"
                                        , err ++ "."
                                        ]
formatError (WithoutPos err) = unwords [ "Error :", err ++ "." ]
