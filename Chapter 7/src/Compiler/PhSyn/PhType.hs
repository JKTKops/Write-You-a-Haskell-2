module Compiler.PhSyn.PhType where

import Utils.Outputable

newtype TVar = LTVar String
  deriving (Show, Eq, Ord)

instance Outputable TVar where
    ppr (LTVar name) = text name

data Type
     = TVar TVar
     | TCon String
     | TArr Type Type
     deriving (Show, Eq, Ord)

infixr `TArr`

instance Outputable Type where
    pprPrec _ (TVar v) = ppr v
    pprPrec _ (TCon c) = text c
    pprPrec p (TArr t1 t2) = parensIf (p > 0)
                             $ pprPrec (p + 1) t1
                             <+> text "->"
                             <+> pprPrec p t2

data Scheme = Forall [TVar] Type
  deriving (Show, Eq, Ord)

instance Outputable Scheme where
    ppr (Forall fvs t) = case fvs of
        [] -> ppr t
        _  -> text "forall" <+> (sep (map ppr fvs) <> char '.') <+> ppr t

typeInt, typeBool :: Type
typeInt  = TCon "Int"
typeBool = TCon "Bool"
