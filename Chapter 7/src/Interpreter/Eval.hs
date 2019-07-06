module Interpreter.Eval where

import Compiler.PhSyn.PhExpr

import Utils.Outputable

import Control.Monad.Identity
import Control.Monad.Reader

import Data.Map (Map)
import qualified Data.Map as Map

data Value
     = VInt Integer
     | VBool Bool
     | VClosure String Expr TermEnv

type TermEnv = Map String Value
-- this will eventually become more complicated
type Interpreter = ReaderT TermEnv Identity

emptyTmEnv :: TermEnv
emptyTmEnv = Map.empty

runInterpreter :: Interpreter a -> TermEnv -> a
runInterpreter = runReader

runEval :: TermEnv -> String -> Expr -> (Value, TermEnv)
runEval env name ex = let res = runInterpreter (eval ex) env
                      in (res, Map.insert name res env)

-- The use of several non-total patterns is justified due to type checking
-- | Evaluate an expression.
eval :: Expr -> Interpreter Value
eval = \case
    Lit (LInt k) -> return $ VInt k
    Lit (LBool k) -> return $ VBool k

    Var x -> do
        termEnv <- ask
        let Just v = Map.lookup x termEnv
        return v

    Op op a b -> do
        VInt a' <- eval a
        VInt b' <- eval b
        return $ binop op a' b'

    Lam x body -> VClosure x body <$> ask -- Create a closure over the current environment

    App fun arg -> do
        VClosure x body clo <- eval fun
        argv <- eval arg
        let env = Map.insert x argv clo
        local (const env) $ eval body

    Let x e body -> do
        e' <- eval e
        local (Map.insert x e') $ eval body

    If cond tr fl -> do
        VBool b <- eval cond
        if b then eval tr else eval fl

    Fix e -> do
        eval (App e $ Fix e)

binop :: Binop -> Integer -> Integer -> Value
binop Add a b = VInt $ a + b
binop Sub a b = VInt $ a - b
binop Mul a b = VInt $ a * b
binop Div a b = VInt $ a `div` b
binop Eql a b = VBool $ a == b

instance Outputable Value where
    ppr (VInt n) = ppr n
    ppr (VBool b) = ppr b
    ppr VClosure{} = text "<<closure>>"
