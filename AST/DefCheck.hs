module AST.DefCheck
( defCheck
)
where

import AST.Types
import Data.List

-- Make sure each global is defined once, check recursive defs are immediate
-- functions, and check main is defined.
defCheck :: Program -> [CheckError]
defCheck program = concat [ checkMultipleDefs program
                          , checkMainExists program
                          , checkLetRecDefsImmediate program
                          ]

checkMainExists :: Program -> [CheckError]
checkMainExists (Program defs)
                | "main" `elem` defNameStrings = []
                | otherwise = [ WithoutPos "No definition for main" ]
                where defNames = programDefNames defs
                      defNameStrings = [ nameString | (_, Var nameString) <- defNames ]

checkMultipleDefs :: Program -> [CheckError]
checkMultipleDefs (Program def) =
        let defNames = programDefNames def
            sortedDefNames = sortBy (\(_, a) (_, b) -> compare a b) defNames
            groupedDefNames = groupBy (\(_, a) (_, b) -> a == b) sortedDefNames
            defNameLocations = [ (name, map fst names) | names@((_, name) : _) <- groupedDefNames ]
        in [ multipleDefError name locations | (name, locations) <- defNameLocations, length locations > 1 ]

multipleDefError :: Var -> [FileLocation] -> CheckError
multipleDefError (Var name) locations = WithoutPos ( unwords [ "Variable"
                                                             , quote name
                                                             , "is has multiple definitions at"
                                                             , intercalate ", " (map show locations)
                                                             ]
                                                   )

programDefNames :: TProgramDef -> [TVar]
programDefNames (_, Main) = []
programDefNames (_, LetDef def restDefs) = defName def : programDefNames restDefs
programDefNames (_, LetRecDefs defs restDefs) = map defName defs ++ programDefNames restDefs

defName :: TDef -> TVar
defName (_, Def name _ _ _) = name

checkLetRecDefsImmediate :: Program -> [CheckError]
checkLetRecDefsImmediate (Program def) = checkLetRecDefsImmediate' def

checkLetRecDefsImmediate' :: TProgramDef -> [CheckError]
checkLetRecDefsImmediate' (_, Main) = []
checkLetRecDefsImmediate' (_, LetDef _ restDefs) = checkLetRecDefsImmediate' restDefs
checkLetRecDefsImmediate' (_, LetRecDefs defs restDefs) = concat [ concat ( map checkDefImmediate defs )
                                                                 , checkLetRecDefsImmediate' restDefs
                                                                 ]

checkDefImmediate :: TDef -> [CheckError]
checkDefImmediate (_, Def _ _ [] (_, Body (_, Lambda _ _ _))) = []
checkDefImmediate (defPos, Def (_, Var defName) _ [] _) =
        [ WithPos defPos
                  ( unwords [ "Recursive definition for"
                            , quote defName
                            , "must be an immediate function definition"
                            ]
                  )
        ]
checkDefImmediate _ = []

quote :: String -> String
quote s = "\"" ++ s ++ "\""
