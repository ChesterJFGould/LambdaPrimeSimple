module AST.Check
( check
)
where

import AST.DefCheck
import AST.TypeCheck
import AST.Types
import qualified TAST.Types as T

import Data.List

check :: Program -> Either String T.Program
check prog = do
             let defErrors = defCheck prog
             case defErrors of
                  [] -> Right ()
                  _ -> Left (formatErrors defErrors)
             let typeErrorsOrProg = typeCheck prog
             case typeErrorsOrProg of
                  Right prog -> Right prog
                  Left typeErrors -> Left (formatErrors typeErrors)

formatErrors :: [CheckError] -> String
formatErrors errors = intercalate "\n\n" (map formatError errors)

formatError :: CheckError -> String
formatError (WithPos pos err) = unwords [ "Error at"
                                        , show pos
                                        , ":"
                                        , err ++ "."
                                        ]
formatError (WithoutPos err) = unwords [ "Error :", err ++ "." ]
