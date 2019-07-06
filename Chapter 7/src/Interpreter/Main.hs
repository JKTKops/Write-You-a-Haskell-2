module Interpreter.Main where

import Compiler.PhSyn.PhExpr
import Compiler.TypeCheck.TcInfer
import Compiler.Parser.Parser
import qualified Compiler.Types.TyEnv as TyEnv

import Interpreter.Eval

import Utils.Outputable

import Data.Monoid
import Data.Map (Map)
import Data.Text.Lazy (Text)
import qualified Data.Map as Map
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import Data.List (isPrefixOf, foldl')

import Control.Monad.Identity
import Control.Monad.State.Strict

import System.Exit
import System.Environment
import System.Console.Repline


---------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------

data IState = IState
  { tyctx :: TyEnv.TyEnv
  , tmctx :: TermEnv
  }

initState :: IState
initState = IState TyEnv.empty emptyTmEnv

type Repl = HaskelineT (StateT IState IO)

handleErr :: Outputable e => Either e a -> Repl a
handleErr (Right val) = return val
handleErr (Left err) = (liftIO . putStrLn . output) err >> abort


----------------------------------------------------------------------------
-- Execution
----------------------------------------------------------------------------

evalDef :: TermEnv -> (String, Expr) -> TermEnv
evalDef env (name, ex) = tmctx'
  where (val, tmctx') = runEval env name ex

exec :: Bool -> Text -> Repl ()
exec update source = do
    st <- get

    -- Parser (returns [(String, Expr)])
    mod <- handleErr $ parseModule "<interactive>" source

    -- Type Inference
    tyctx' <- handleErr $ inferTop (tyctx st) mod

    -- Create updated environment
    let st' = st { tmctx = foldl' evalDef (tmctx st) mod
                 , tyctx = tyctx' <> (tyctx st)
                 }

    -- Update the interpreter state
    when update $ put st'

    -- If a value was entered, print it
    case lookup "it" mod of
        Nothing -> return ()
        Just ex -> let (val, _) = runEval (tmctx st') "it" ex
                   in showOutput val st'

showOutput :: Outputable a => a -> IState -> Repl ()
showOutput arg st = case TyEnv.lookup "it" (tyctx st) of
    Just ty -> liftIO $ print $ ppr arg <+> text "::" <+> ppr ty
    Nothing -> return ()

cmd :: String -> Repl ()
cmd source = exec True $ L.pack source

---------------------------------------------------------------------------
-- Commands
---------------------------------------------------------------------------

-- :b[rowse]
browse :: [String] -> Repl ()
browse _ = do
    st <- get
    liftIO $ print $ ppr $ tyctx st

-- :l[oad]
load :: [String] -> Repl ()
load args = do
    contents <- liftIO $ mapM L.readFile args
    mapM_ (exec True) contents

-- :t[ype]
typeof :: [String] -> Repl ()
typeof args = do
    st <- get
    let expr = unwords args
    case TyEnv.lookup expr (tyctx st) of
        Just val -> liftIO $ print $ text expr <+> text "::" <+> ppr val
        Nothing -> do
            expr' <- handleErr $ parseExpr $ L.pack expr
            scheme <- handleErr $ inferExpr (tyctx st) expr'
            liftIO $ print $ text expr <+> text "::" <+> ppr scheme

-- :q[uit]
quit :: a -> Repl ()
quit _ = liftIO $ exitSuccess

---------------------------------------------------------------------------
-- Interactive Shell
---------------------------------------------------------------------------

-- Prefix tab completion
defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher = [(":load", fileCompleter)]

-- Default tab completer
comp :: WordCompleter (StateT IState IO)
comp n = do
    let cmds = [ ":l", ":load"
               , ":t", ":type"
               , ":b", "browse" -- in GHCi, ':b' is ':breakpoint'
               , ":q", ":quit"
               ]
    TyEnv.TypeEnv ctx <- gets tyctx
    let defs = Map.keys ctx
    return $ filter (isPrefixOf n) (cmds ++ defs)

options :: [(String, [String] -> Repl ())]
options = [ ("load"   , load)
          , ("browse" , browse)
          , ("quit"   , quit)
          , ("type"   , typeof)
          ]

completer :: CompleterStyle (StateT IState IO)
completer = Prefix (wordCompleter comp) defaultMatcher

-----------------------------------------------------------------------------
-- Toplevel
-----------------------------------------------------------------------------

shell :: Repl a -> IO ()
shell startup = flip evalStateT initState
             $ evalRepl (return "Poly> ") cmd options (Just ':') completer startup

main :: IO ()
main = do
    args <- getArgs
    case args of
        []     -> shell $ return ()
        [fname] -> shell $ load [fname]
        ["test", fname] -> shell $ load [fname] >> browse [] >> quit ()
        _ -> putStrLn $ "Invalid arguments"
