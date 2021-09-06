module AST.Parse
( AST.Parse.parse
)
where

import AST.Types
import Compiler.Types

import Data.Foldable
import Data.Void
import Text.Megaparsec  as M
import Text.Megaparsec.Char hiding (hspace, symbolChar)
import Text.Megaparsec.Char.Lexer hiding (space, symbol)

type Parser a = Parsec Void String a

binops :: ([Parser TExpr -> Parser TExpr], Parser TExpr)
binops = ( [ binopsl [ ("<=", BinOp (BRelOp Lte))
                     , (">=", BinOp (BRelOp Gte))
                     , ("/=", BinOp (BRelOp Neq))
                     , ("==", BinOp (BRelOp Eq))
                     , ("<", BinOp (BRelOp Lt))
                     , (">", BinOp (BRelOp Gt))
                     ]
           , binopsl [ ("+", BinOp (BNumOp Add))
                     , ("-", BinOp (BNumOp Sub))
                     ]
           , binopsl [ ("*", BinOp (BNumOp Mul))
                     , ("/", BinOp (BNumOp Div))
                     , ("%", BinOp (BNumOp Mod))
                     ]
           , application
           ]
         , argument
         )

typeOps :: ([Parser TTypeAnn -> Parser TTypeAnn], Parser TTypeAnn)
typeOps = ( [ binopsr [ ("->", TAFunc) ] ]
          , typeArgument
          )

keywords :: [String]
keywords = ["if", "then", "else", "let", "in", "true", "false", "letrec"]

parse :: String -> String -> Either String Program
parse fileName text = either (Left . errorBundlePretty) Right (parseProg fileName text)

parseProg :: String -> String -> Either (ParseErrorBundle String Void) Program
parseProg fileName text = M.parse prog fileName text

prog :: Parser Program
prog = Program <$> progDef

progDef :: Parser TProgramDef
progDef = tag ( asum [ try letRecDef
                     , letDef
                     , hspace >> eof >> return Main
                     ]
              )

letDef :: Parser ProgramDef
letDef = LetDef <$> (hspace >> def)
                <*> (hspace >> progDef)

letRecDef :: Parser ProgramDef
letRecDef =
        do
        char '['
        hspace
        symbol "letrec"
        hspace
        defs <- def `sepEndBy1` hspace
        hspace
        char ']'
        LetRecDefs defs <$> progDef

def :: Parser TDef
def =
        tag ( do
              char '['
              hspace
              name <- var
              hspace
              char ':'
              hspace
              defType <- type'
              hspace
              (args, body) <- asum [ do
                                     char '='
                                     hspace
                                     defBody <- body
                                     return ([], defBody)
                                   , do
                                     char '|'
                                     hspace
                                     args <- var `sepEndBy1` hspace
                                     hspace
                                     char '='
                                     hspace
                                     defBody <- body
                                     return (args, defBody)
                                   ]
              hspace
              char ']'
              return (Def name defType args body)
            )

tag :: Parser a -> Parser (Tagged a)
tag p =
        do
        loc <- fileLocation
        val <- p
        return (loc, val)

fileLocation :: Parser FileLocation
fileLocation =
        do
        SourcePos name linePos columnPos <- getSourcePos
        let lineNum = unPos linePos
            columnNum = unPos columnPos
            loc = FileLocation name lineNum columnNum
        return loc

symbol :: String -> Parser String
symbol s = string s <* notFollowedBy symbolChar

var :: Parser TVar
var =
        tag ( do
              notFollowedBy (asum (map symbol keywords))
              firstChar <- letterChar
              restChars <- many symbolChar
              return (Var (firstChar : restChars))
            )

type' :: Parser TTypeAnn
type' = precedence typeOps

typeArgument :: Parser TTypeAnn
typeArgument = tag ( asum [ symbol "Int" >> return TAInt
                          , symbol "Bool" >> return TABool
                          ]
                   )

body :: Parser TBody
body = tag ( Body <$> expr )

expr :: Parser TExpr
expr = asum [ tag let'
            , tag if'
            , tag lambda
            , precedence binops
            ]

let' :: Parser Expr
let' = Let <$> (symbol "let" >> hspace >> var)
           <*> (hspace >> char '=' >> hspace >> expr)
           <*> (hspace >> symbol "in" >> hspace >> expr)

if' :: Parser Expr
if' = If <$> (symbol "if" >> hspace >> expr)
         <*> (hspace >> symbol "then" >> hspace >> expr)
         <*> (hspace >> symbol "else" >> hspace >> expr)

lambda :: Parser Expr
lambda = Lambda <$> (char 'λ' >> hspace >> var)
               <*> (hspace >> char ':' >> hspace >> type')
               <*> (hspace >> char '.' >> hspace >> expr)

application :: Parser TExpr -> Parser TExpr
application p =
        do
        f <- p
        application' f p

application' :: TExpr -> Parser TExpr -> Parser TExpr
application' f p =
        try ( do
              f' <- tag ( do
                          hspace
                          arg <- p
                          return (Apply f arg)
                        )
              application' f' p
            )
        <|> return f

argument :: Parser TExpr
argument = asum [ tag ( Value <$> value )
                , char '(' *> hspace *> expr <* hspace <* char ')'
                ]

value :: Parser TValue
value = tag ( asum [ int
                   , bool
                   , VVar <$> var
                   ]
            )

int :: Parser Value
int = Int <$> decimal

bool :: Parser Value
bool = asum [ symbol "true" >> return (Bool True)
            , symbol "false" >> return (Bool False)
            ]

precedence :: ([Parser a -> Parser a], Parser a) -> Parser a
precedence (ops, base) = foldr ($) base ops

binopsl :: [(String, Tagged a -> Tagged a -> a)] -> Parser (Tagged a) -> Parser (Tagged a)
binopsl tokenOps p =
        do
        let ops = asum (map binop tokenOps)
        left <- p
        binopsl' left ops p

binopsl' :: Tagged a -> Parser (Tagged a -> Tagged a -> a) -> Parser (Tagged a) -> Parser (Tagged a)
binopsl' left opP p =
        do
        try ( do
              left' <- tag ( do
                             hspace
                             op <- opP
                             hspace
                             right <- p
                             return (op left right)
                           )

              binopsl' left' opP p
            )
        <|> return left

binopsr :: [(String, Tagged a -> Tagged a -> a)] -> Parser (Tagged a) -> Parser (Tagged a)
binopsr tokenOps p =
        do
        let ops = asum (map binop tokenOps)
        binopsr' ops p

binopsr' :: Parser (Tagged a -> Tagged a -> a) -> Parser (Tagged a) -> Parser (Tagged a)
binopsr' opP p =
        do
        left <- p
        ( try ( tag ( do
                      hspace
                      op <- opP
                      hspace
                      right <- binopsr' opP p
                      return (op left right)
                  )
            )
          <|> return left
         )

binop :: (String, Tagged a -> Tagged a -> a) -> Parser (Tagged a -> Tagged a -> a)
binop (token, op) = symbol token >> return op

symbolChar :: Parser Char
symbolChar = alphaNumChar <|> char '\'' <|> char '?'

hspace :: Parser ()
hspace = hidden space
