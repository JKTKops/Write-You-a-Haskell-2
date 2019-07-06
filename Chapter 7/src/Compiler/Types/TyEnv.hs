module Compiler.Types.TyEnv
    ( TyEnv(..)
    , empty
    , lookup
    , remove
    , extend
    , extends
    , merge
    , mergeEnvs
    , singleton
    , keys
    , fromList
    , toList
    ) where

import Prelude hiding (lookup)

import Compiler.PhSyn.PhExpr
import Compiler.PhSyn.PhType

import Utils.Outputable

import Data.Monoid
import Data.Foldable hiding (toList)
import qualified Data.Map as Map

data TyEnv = TypeEnv { types :: Map.Map Name Scheme }
  deriving (Eq, Show)

empty :: TyEnv
empty = TypeEnv Map.empty

extend :: TyEnv -> (Name, Scheme) -> TyEnv
extend (TypeEnv env) (x, s) = TypeEnv $ Map.insert x s env

remove :: TyEnv -> Name -> TyEnv
remove (TypeEnv env) var = TypeEnv $ Map.delete var env

extends :: TyEnv -> [(Name, Scheme)] -> TyEnv
extends (TypeEnv env) xs = TypeEnv $ Map.union (Map.fromList xs) env

lookup :: Name -> TyEnv -> Maybe Scheme
lookup key (TypeEnv env) = Map.lookup key env

merge :: TyEnv -> TyEnv -> TyEnv
merge (TypeEnv a) (TypeEnv b) = TypeEnv $ a <> b

mergeEnvs :: [TyEnv] -> TyEnv
mergeEnvs = mconcat

singleton :: Name -> Scheme -> TyEnv
singleton x y = TypeEnv $ Map.singleton x y

keys :: TyEnv -> [Name]
keys (TypeEnv env) = Map.keys env

fromList :: [(Name, Scheme)] -> TyEnv
fromList xs = TypeEnv $ Map.fromList xs

toList :: TyEnv -> [(Name, Scheme)]
toList (TypeEnv env) = Map.toList env

instance Semigroup TyEnv where
    (<>) = merge

instance Monoid TyEnv where
    mempty = empty
    mappend = (<>)

instance Outputable TyEnv where
    ppr env = let types = toList env
              in vcat $ [ text name <+> text "::" <+> ppr scheme
                        | (name, scheme) <- types
                        ]
