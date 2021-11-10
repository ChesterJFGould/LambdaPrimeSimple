module AST.TypeCheck
( typeCheck
)
where

import AST.Types
import qualified TAST.Types as T

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as M

type Check a = ExceptT [CheckError] (State (M.Map Var T.Type)) a

execCheck :: Check a -> Either [CheckError] a
execCheck computation = evalState (runExceptT computation) M.empty

typeCheck :: Program -> Either [CheckError] T.Program
typeCheck (Program def) = T.Program <$> execCheck (typeCheckProgramDef def)

typeCheckProgramDef :: TProgramDef -> Check T.TProgramDef
typeCheckProgramDef = _
