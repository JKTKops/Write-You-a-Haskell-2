module Compiler.TypeCheck.TcInfer
    ( Constraint
    , TypeError(..)
    , Subst(..)
    , inferExpr
    , inferTop
    , constraintsExpr
    ) where

import Compiler.Types.TyEnv
import Compiler.PhSyn.PhType
import Compiler.PhSyn.PhExpr

import Utils.Outputable

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Supply
import Control.Monad.Writer
import Control.Monad.Identity

import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Function ((&))

type Infer = ReaderT TyEnv
             (WriterT [Constraint]
             (SupplyT Type
             (Except TypeError)))

type Constraint = (Type, Type)
type Unifier = (Subst, [Constraint])

type Solve = StateT Unifier (Except TypeError)

newtype Subst = Subst (Map TVar Type)
  deriving (Eq, Ord, Show, Semigroup, Monoid)

class Substitutable a where
    apply :: Subst -> a -> a
    ftv   :: a -> Set TVar

instance Substitutable Type where
    apply _ (TCon a) = TCon a
    apply (Subst s) t@(TVar a) = Map.findWithDefault t a s
    apply s (TArr t1 t2) = apply s t1 `TArr` apply s t2

    ftv TCon{}       = Set.empty
    ftv (TVar a)     = Set.singleton a
    ftv (TArr t1 t2) = ftv t1 `Set.union` ftv t2

instance Substitutable Scheme where
    apply (Subst s) (Forall as t) = Forall as $ apply s' t
      where s' = Subst $ foldr Map.delete s as
    ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance Substitutable Constraint where
    apply s (t1, t2) = (apply s t1, apply s t2)
    ftv (t1, t2) = ftv t1 <> ftv t2

instance Substitutable a => Substitutable [a] where
    apply = map . apply
    ftv = foldr (Set.union . ftv) Set.empty

instance Substitutable TyEnv where
    apply s (TypeEnv env) = TypeEnv $ apply s <$> env
    ftv (TypeEnv env) = ftv $ Map.elems env

data TypeError
     = UnificationFail Type Type
     | InfiniteType TVar Type
     | UnboundVariable String
     | Ambigious [Constraint]
     | UnificationMismatch [Type] [Type]

------------------------------------------------------------------------
-- Inference
------------------------------------------------------------------------

runInfer :: TyEnv -> Infer Type -> Either TypeError (Type, [Constraint])
runInfer env m = runReaderT m env
                 & runWriterT
                 & flip runSupplyT (map TVar initLTVarSupply)
                 & runExcept

initLTVarSupply :: [TVar]
initLTVarSupply = map LTVar $ [1..] >>= flip replicateM ['a'..'z']

-- Solves for type of an expression in given environment
inferExpr :: TyEnv -> Expr -> Either TypeError Scheme
inferExpr env e = do
    (ty, cs) <- runInfer env (infer e)
    subst <- runSolve cs
    return $ closeOver $ apply subst ty

-- Solves for type of an expression and also returns
-- the constraints, substitution, and pre-substitution type
constraintsExpr :: TyEnv -> Expr -> Either TypeError ([Constraint], Subst, Type, Scheme)
constraintsExpr env e = do
    (ty, cs) <- runInfer env (infer e)
    subst <- runSolve cs
    let sc = closeOver $ apply subst ty
    return (cs, subst, ty, sc)

closeOver :: Type -> Scheme
closeOver = normalize . generalize Compiler.Types.TyEnv.empty

normalize :: Scheme -> Scheme
normalize (Forall _ body) = Forall (map snd ord) (normtype body)
  where
    -- Create assoc. list mapping free variables back to the 'normal'
    -- type variables 'a', 'b', 'c', ...
    ord = zip (nub $ fv body) initLTVarSupply

    fv (TVar a) = [a]
    fv (TArr a b) = fv a ++ fv b
    fv (TCon _) = []

    normtype (TArr a b) = TArr (normtype a) (normtype b)
    normtype (TCon a) = TCon a
    normtype (TVar a) = case Prelude.lookup a ord of
        Just x -> TVar x
        Nothing -> error "Type variable not in signature"
          -- this can't happen; this would require (not.elem) a fv body,
          -- but a is a free variable of the body and therefore a `elem` fv body.

generalize :: TyEnv -> Type -> Scheme
generalize env t = Forall as t
  where as = Set.toList $ ftv t `Set.difference` ftv env

inEnv :: (Name, Scheme) -> Infer a -> Infer a
inEnv (x, sc) m = do
    let scope e = (remove e x) `extend` (x, sc)
    local scope m

lookupEnv :: Name -> Infer Type
lookupEnv x = do
    TypeEnv env <- ask
    case Map.lookup x env of
        Nothing -> throwError $ UnboundVariable x
        Just s  -> instantiate s

instantiate :: Scheme -> Infer Type
instantiate (Forall as t) = do
    as' <- mapM (const supply) as
    let s = Subst $ Map.fromList $ zip as as'
    return $ apply s t

ops :: Binop -> Type
ops = \case
    Add -> intBinopType
    Mul -> intBinopType
    Sub -> intBinopType
    Div -> intBinopType
    Eql -> intBinPredType
  where intBinopType = typeInt `TArr` typeInt `TArr` typeInt
        intBinPredType = typeInt `TArr` typeInt `TArr` typeBool

infer :: Expr -> Infer Type
infer = \case
    Lit (LInt _)  -> return typeInt
    Lit (LBool _) -> return typeBool

    Var x -> lookupEnv x

    Lam x e -> do
        tv <- supply
        t <- inEnv (x, Forall [] tv) $ infer e
        return $ tv `TArr` t

    App e1 e2 -> do
        t1 <- infer e1
        t2 <- infer e2
        tv <- supply
        tiUnify t1 $ t2 `TArr` tv
        return tv

    Let x e1 e2 -> do
        env <- ask
        (t1, constraints) <- listen $ infer e1
        subst <- liftEither $ runSolve constraints
        let t1' = apply subst t1
        let sc = generalize env t1' -- let-generalization
        t2 <- inEnv (x, sc) $ infer e2
        return t2

    Fix e -> do
        t1 <- infer e
        tv <- supply
        tiUnify (tv `TArr` tv) t1
        return tv

    Op op e1 e2 -> do
        t1 <- infer e1
        t2 <- infer e2
        tv <- supply
        let u1 = t1 `TArr` t2 `TArr` tv
            u2 = ops op
        tiUnify u1 u2
        return tv

    If cond tr fl -> do
        t1 <- infer cond
        tiUnify t1 typeBool
        t2 <- infer tr
        t3 <- infer fl
        tiUnify t2 t3
        return t2

tiUnify :: Type -> Type -> Infer ()
tiUnify t1 t2 = tell [(t1, t2)]

inferTop :: TyEnv -> [(String, Expr)] -> Either TypeError TyEnv
inferTop env [] = Right env
inferTop env ((name, e):xs) = do
    ty <- inferExpr env e
    inferTop (extend env (name, ty)) xs

----------------------------------------------------------------------
-- Constraint Solver
----------------------------------------------------------------------

runSolve :: [Constraint] -> Either TypeError Subst
runSolve cs = evalStateT solver (mempty, cs) & runExcept

solver :: Solve Subst
solver = do
    (su, cs) <- get
    case cs of
        [] -> return su
        ((t1, t2): cs') -> do
            su' <- tcUnify t1 t2
            put $ (su' `compose` su, apply su' cs')
            solver

tcUnify :: Type -> Type -> Solve Subst
tcUnify t1 t2 | t1 == t2 = return mempty
tcUnify (TVar v) t = v `tcUnifyWith` t
tcUnify t (TVar v) = v `tcUnifyWith` t
tcUnify (TArr t1 t2) (TArr t3 t4) = tcUnifyMany [t1, t2] [t3, t4]
tcUnify t1 t2 = throwError $ UnificationFail t1 t2

tcUnifyMany :: [Type] -> [Type] -> Solve Subst
tcUnifyMany [] [] = return mempty
tcUnifyMany (t1:ts1) (t2:ts2) = do
    subst1 <- tcUnify t1 t2
    subst2 <- tcUnifyMany (apply subst1 ts1) (apply subst1 ts2)
    return $ subst1 `compose` subst2
tcUnifyMany t1 t2 = throwError $ UnificationMismatch t1 t2 -- lists of different size

tcUnifyWith :: TVar -> Type -> Solve Subst
tcUnifyWith a t | t == TVar a     = return mempty
                | occursCheck a t = throwError $ InfiniteType a t
                | otherwise       = return $ Subst $ Map.singleton a t

occursCheck :: Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

-- | compose substitutions
compose :: Subst -> Subst -> Subst
compose (Subst s1) (Subst s2) = Subst $ Map.map (apply $ Subst s1) s2 `Map.union` s1

------------------------------------------------------------------------
-- Outputable Instances
------------------------------------------------------------------------

instance Outputable Subst where
    ppr (Subst s) = vcat $ map pprSub $ Map.toList s
      where pprSub (a, b) = ppr a <+> text "~" <+> ppr b

instance Outputable TypeError where
    ppr (UnificationFail a b) = text "Cannot unify type `" <> ppr a
                                <> text "' with type `" <> ppr b <> char '\''
    ppr (InfiniteType (LTVar a) t) =
        text "Occurs check: Cannot construct the infinite type:"
        <+> text a <+> char '~' <+> ppr t
    ppr (Ambigious cs) =
        vcat [ text "Couldn't match actual type `" <> ppr b
               <> text "' with expected type `" <> ppr a <> char '\''
             | (a, b) <- cs
             ]
    ppr (UnboundVariable a) = text "Variable not in scope:" <+> text a
