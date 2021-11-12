module AST.Parse
( AST.Parse.parse
)
where

import AST.Types
import Compiler.Types

import Data.Foldable
import Data.Void
import Text.Megaparsec as M
import Text.Megaparsec.Char hiding (hspace, symbolChar)
import Text.Megaparsec.Char.Lexer hiding (space, symbol)

type Parser a = Parsec Void String a

exprOps :: ([Parser TExpr -> Parser TExpr], Parser TExpr)
exprOps = ( [ binopsl [ (symbol "<=", BinOp (Rel Lte))
                      , (symbol ">=", BinOp (Rel Gte))
                      , (symbol "==", BinOp (Rel Eq))
                      , (symbol "<", BinOp (Rel Lt))
                      , (symbol ">", BinOp (Rel Gt))
                      , (symbol "/=", BinOp (Rel Neq))
                      ]
            , binopsl [ (symbol "+", BinOp (Num Add))
                      , (symbol "-", BinOp (Num Sub))
                      ]
            , binopsl [ (symbol "*", BinOp (Num Mul))
                      , (symbol "/", BinOp (Num Div))
                      , (symbol "%", BinOp (Num Mod))
                      ]
            , application
            ]
          , argumentExpr
          )

typeOps :: ([Parser TTypeAnn -> Parser TTypeAnn], Parser TTypeAnn)
typeOps = ( [ binopsr [ (symbol "->", TAFunc) ] ]
          , argumentType
          )

keywords :: [String]
keywords = [ "if", "then", "else", "let", "in", "val", "fun", "and", "True", "False" ]

parse :: String -> String -> Either String Program
parse fileName text = either (Left . errorBundlePretty)
                             Right
                             (parseProgram fileName text)

parseProgram :: String -> String -> Either (ParseErrorBundle String Void) Program
parseProgram fileName text = M.parse program fileName text

program :: Parser Program
program = Program <$> programDef

programDef :: Parser TProgramDef
programDef = hspace >> tag ( asum [ LetValue <$> value <*> programDef
                                  , LetFuncs <$> funcs <*> programDef
                                  , eof >> return Main
                                  ]
                           )

value :: Parser TValue
value = tag ( Value <$> (symbol "val" >> hspace >> var)
                    <*> (hspace >> typeAnn)
                    <*> (hspace >> char '=' >> hspace >> body)
            )
            <?> "value definition"

funcs :: Parser [TFunc]
funcs = do
        first <- funDef "fun"
        hspace
        rest <- many (funDef "and" <* hspace)
        return (first : rest)

funDef :: String -> Parser TFunc
funDef keyword = tag ( Func <$> (symbol keyword >> hspace >> var)
                            <*> (hspace >> typeAnn)
                            <*> (hspace >> char '|' >> hspace >> some ((var <?> "argument variable") <* hspace))
                            <*> (hspace >> char '=' >> hspace >> body)
                     )
                     <?> "function definition"

typeAnn :: Parser TTypeAnn
typeAnn = char ':' >> hspace >> typeP
          <?> "type annotation"

typeP :: Parser TTypeAnn
typeP = precedence typeOps

argumentType :: Parser TTypeAnn
argumentType = asum [ parens typeP
                    , baseType
                    ]

baseType :: Parser TTypeAnn
baseType = tag ( asum [ symbol "Int" >> return TAInt
                      , symbol "Bool" >> return TABool
                      ]
               )
               <?> "type"

body :: Parser TBody
body = tag ( Body <$> expr)

expr :: Parser TExpr
expr = asum [ exprStmt
            , precedence exprOps
            ]
       <?> "expression"

exprStmt :: Parser TExpr
exprStmt = tag ( asum [ ifP
                      , letP
                      , lambdaP
                      ]
               )

ifP :: Parser Expr
ifP = If <$> (symbol "if" >> hspace >> expr)
         <*> (hspace >> symbol "then" >> hspace >> expr)
         <*> (hspace >> symbol "else" >> hspace >> expr)
      <?> "if expression"

letP :: Parser Expr
letP = Let <$> (symbol "let" >> hspace >> var)
           <*> (hspace >> char '=' >> hspace >> expr)
           <*> (hspace >> symbol "in" >> hspace >> expr)
       <?> "let expression"

lambdaP :: Parser Expr
lambdaP = Lambda <$> (char 'λ' >> hspace >> var)
                 <*> (hspace >> typeAnn)
                 <*> (hspace >> char '.' >> hspace >> expr)
          <?> "lambda expression"

application :: Parser TExpr -> Parser TExpr
application p =
        do
        f <- p
        application' f p

application' :: TExpr -> Parser TExpr -> Parser TExpr
application' f p =
        do
        f' <- try ( tag ( do
                          hspace
                          arg <- p <?> "function argument"
                          return (Apply f arg)
                        )
                  )
        application' f' p
        <|> return f

argumentExpr :: Parser TExpr
argumentExpr = asum [ exprStmt
                    , parens expr
                    , tag atomP
                    ]

parens :: Parser a -> Parser a
parens p = char '(' *> hspace *> p <* hspace <* char ')'

atomP :: Parser Expr
atomP = Atom <$> tag ( asum [ intP
                            , boolP
                            , VVar <$> var
                            ]
                     )

intP :: Parser Atom
intP = Int <$> decimal
       <?> "int"

boolP :: Parser Atom
boolP = Bool <$> asum [ symbol "True" >> return True
                      , symbol "False" >> return False
                      ]
        <?> "bool"

var :: Parser TVar
var = tag ( do
            notFollowedBy (asum (map symbol keywords))
            firstChar <- letterChar
            restChars <- many symbolChar
            return (Var (firstChar : restChars))
          )
      <?> "variable"

tag :: Parser a -> Parser (Tagged a)
tag parser =
        do
        loc <- fileLocation
        thing <- parser
        return (loc, thing)

fileLocation :: Parser FileLocation
fileLocation =
        do
        SourcePos name line column <- getSourcePos
        let line' = unPos line
            column' = unPos column
            loc = FileLocation name line' column'
        return loc

symbolChar :: Parser Char
symbolChar = alphaNumChar <|> char '\'' <|> char '?'

symbol :: String -> Parser String
symbol s = string s <* notFollowedBy symbolChar

hspace :: Parser ()
hspace = hidden space

precedence :: ([Parser a -> Parser a], Parser a) -> Parser a
precedence (ops, base) = foldr ($) base ops

binopsl :: [(Parser w, Tagged a -> Tagged a -> a)] -> Parser (Tagged a) -> Parser (Tagged a)
binopsl ops p =
        do
        let opP = asum (map makeBinop ops)
        initial <- p
        binopsl' initial opP p

binopsl' :: Tagged a -> Parser (Tagged a -> Tagged a -> a) -> Parser (Tagged a) -> Parser (Tagged a)
binopsl' left opP p =
        do
        left' <- do
                 (loc, op) <- try ( hspace >> tag opP)
                 hspace
                 right <- p
                 return (loc, op left right)
        binopsl' left' opP p
        <|> return left

binopsr :: [(Parser w, Tagged a -> Tagged a -> a)] -> Parser (Tagged a) -> Parser (Tagged a)
binopsr ops p =
        do
        let opsP = asum (map makeBinop ops)
        binopsr' opsP p

binopsr' :: Parser (Tagged a -> Tagged a -> a) -> Parser (Tagged a) -> Parser (Tagged a)
binopsr' opP p =
        do
        left <- p
        ( do
          (loc, op) <- try (hspace >> tag opP)
          hspace
          right <- binopsr' opP p
          return (loc, op left right)
          <|> return left
         )


makeBinop :: (Parser w, Tagged a -> Tagged a -> a) -> Parser (Tagged a -> Tagged a -> a)
makeBinop (p, fun) = p >> return fun
