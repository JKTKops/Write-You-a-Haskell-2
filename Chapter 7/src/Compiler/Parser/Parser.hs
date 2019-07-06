module Compiler.Parser.Parser
    ( parseExpr
    , parseModule
    ) where

import Text.Parsec
import Text.Parsec.Text.Lazy (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok
import qualified Data.Text.Lazy as L

import Control.Monad (when)

import Compiler.Parser.Lexer
import Compiler.PhSyn.PhExpr

variable :: Parser Expr
variable = identifier >>= return . Var

number :: Parser Expr
number = integer >>= return . Lit . LInt . fromIntegral

bool :: Parser Expr
bool =  (reserved "True" >> (return . Lit . LBool) True)
    <|> (reserved "False" >> (return . Lit . LBool) False)

fix :: Parser Expr
fix = do
    reserved "fix"
    x <- expr
    return $ Fix x

lambda :: Parser Expr
lambda = do
    reservedOp "\\"
    args <- many identifier
    reservedOp "->"
    body <- expr
    return $ foldr Lam body args

letin :: Bool -> Parser Expr
letin isrec = do
    reserved "let"
    when isrec $ reserved "rec"
    x <- identifier
    args <- many identifier
    reservedOp "="
    e1 <- expr
    reserved "in"
    e2 <- expr
    let rhs = foldr Lam e1 args
    if isrec
       then return $ Let x (Fix rhs) e2
       else return $ Let x rhs e2

ifthen :: Parser Expr
ifthen = do
    reserved "if"
    cond <- expr
    reserved "then"
    tr <- expr
    reserved "else"
    fl <- expr
    return $ If cond tr fl

aexp :: Parser Expr
aexp =  parens expr
    <|> bool
    <|> number
    <|> ifthen
    <|> fix
    <|> try (letin True)
    <|> letin False
    <|> lambda
    <|> variable

term :: Parser Expr
term = do
    x  <- aexp
    xs <- many aexp
    return $ foldl App x xs

infixOp :: String -> (a -> a -> a) -> Ex.Assoc -> Op a
infixOp x f a = Ex.Infix (reservedOp x >> return f) a

table :: Operators Expr
table =
    [ [ infixOp "*" (Op Mul) Ex.AssocLeft
      , infixOp "/" (Op Div) Ex.AssocNone
      ]
    , [ infixOp "+" (Op Add) Ex.AssocLeft
      , infixOp "-" (Op Sub) Ex.AssocLeft
      ]
    , [ infixOp "==" (Op Eql) Ex.AssocLeft
      ]
    ]

expr :: Parser Expr
expr = Ex.buildExpressionParser table term

type Binding = (String, Expr)

-- Top level - shouldn't be parsed as let/in since no body
letdecl :: Parser Binding
letdecl = do
    reserved "let"
    name <- identifier
    args <- many identifier
    reservedOp "="
    body <- expr
    return (name, foldr Lam body args)

letrecdecl :: Parser Binding
letrecdecl = do
    reserved "let"
    reserved "rec"
    name <- identifier
    args <- many identifier
    reservedOp "="
    body <- expr
    return (name, Fix $ foldr Lam body (name:args)) -- the function becomes the arg to fix

val :: Parser Binding
val = do
    ex <- expr
    return ("it", ex)

decl :: Parser Binding
decl = try val <|> try letrecdecl <|> letdecl

top :: Parser Binding
top = do
    x <- decl
    optional semi
    return x

modl :: Parser [Binding]
modl = many top

parseExpr :: L.Text -> Either ParseError Expr
parseExpr input = parse (contents expr) "<interactive>" input

parseModule :: FilePath -> L.Text -> Either ParseError [Binding]
parseModule fname input = parse (contents modl) fname input
