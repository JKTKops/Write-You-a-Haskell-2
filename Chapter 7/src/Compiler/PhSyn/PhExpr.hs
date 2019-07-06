module Compiler.PhSyn.PhExpr where

import Utils.Outputable

type Name = String

data Expr
     = Var Name               -- regular old variable
     | App Expr Expr          -- application
     | Lam Name Expr          -- lambda abstractions
     | Let Name Expr Expr     -- Singular let-binding
     | Lit Lit                -- literals
     | If Expr Expr Expr      -- conditional
     | Fix Expr               -- fixpoint
     | Op Binop Expr Expr     -- Built-in operators
     deriving (Eq, Ord, Show)

data Lit
     = LInt Integer
     | LBool Bool
     deriving (Show, Eq, Ord)

data Binop = Add | Sub | Mul | Div | Eql
  deriving (Show, Eq, Ord)

data Program = Program [Decl] Expr
  deriving Eq

type Decl = (Name, Expr)

instance Outputable Expr where
    pprPrec _ (Var a) = text a
    pprPrec p (App a b) = parensIf (p > 0) $ pprPrec (p+1) a <+> pprPrec p b
    pprPrec p (Lam a b) = (char '\\' <> pprPrec p a) <+> text "->" <+> pprPrec p b
    pprPrec p (Let a b c) = (text "let"
                            <> pprPrec p a) <+> text "=" <+> pprPrec p b
                            <+> text "in" <+> pprPrec p c
    pprPrec _ (Lit a) = ppr a
    pprPrec p (Op o a b) = parensIf (p>0) $ pprPrec p a <+> ppr o <+> pprPrec p b
    pprPrec p (Fix a) = parensIf (p > 0) $ text "fix" <+> pprPrec p a
    pprPrec p (If a b c) = text "if" <+> ppr a <+>
                           text "then" <+> ppr b <+>
                           text "else" <+> ppr c

instance Outputable Lit where
    ppr (LInt i) = ppr i
    ppr (LBool b) = ppr b

instance Outputable Binop where
    ppr Add = text "+"
    ppr Sub = text "-"
    ppr Mul = text "*"
    ppr Div = text "/"
    ppr Eql = text "=="
