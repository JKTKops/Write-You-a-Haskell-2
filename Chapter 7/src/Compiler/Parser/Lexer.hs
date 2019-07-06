module Compiler.Parser.Lexer where

import           Text.Parsec
import           Text.Parsec.Text.Lazy
import qualified Data.Text.Lazy as L
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Expr as Ex

import Data.Functor.Identity

type Op a = Ex.Operator L.Text () Identity a
type Operators a = Ex.OperatorTable L.Text () Identity a

reservedNames :: [String]
reservedNames =
    [ "let"
    , "in"
    , "fix"
    , "rec"
    , "if"
    , "then"
    , "else"
    ]

reservedOps :: [String]
reservedOps =
    [ "->"
    , "\\"
    , "+"
    , "-"
    , "*"
    , "/"
    , "="
    ]

lexer :: Tok.GenTokenParser L.Text () Identity
lexer = Tok.makeTokenParser $ langDef
  where
    langDef = Tok.LanguageDef
      { Tok.commentStart    = "{-"
      , Tok.commentEnd      = "-}"
      , Tok.commentLine     = "--"
      , Tok.nestedComments  = True
      , Tok.identStart      = letter
      , Tok.identLetter     = alphaNum <|> oneOf "_'"
      , Tok.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
      , Tok.opLetter        = Tok.opStart langDef
      , Tok.reservedNames   = reservedNames
      , Tok.reservedOpNames = reservedOps
      , Tok.caseSensitive   = True
      }

lexeme :: Parser a -> Parser a
lexeme = Tok.lexeme lexer

integer :: Parser Integer
integer = Tok.natural lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Tok.identifier lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

braces :: Parser a -> Parser a
braces = Tok.braces lexer

angles :: Parser a -> Parser a
angles = Tok.angles lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

semiSep1 :: Parser a -> Parser [a]
semiSep1 = Tok.semiSep1 lexer

semi :: Parser String
semi = Tok.semi lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

commaSep1 :: Parser a -> Parser [a]
commaSep1 = Tok.commaSep1 lexer

comma :: Parser String
comma = Tok.comma lexer

colon :: Parser String
colon = Tok.colon lexer

contents :: Parser a -> Parser a
contents p = do
    Tok.whiteSpace lexer
    r <- p
    eof
    return r
