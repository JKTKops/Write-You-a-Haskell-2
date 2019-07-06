# Additions to Poly
<h2> The SupplyT Monad Transformer </h2>
As explained previously, a monad transformer is a way of adding new capabilities onto an existing monad, where of course monads are like "first-class actions." Frequently throughout compiling, we'll need to grab fresh names (and possibly numbers, etc). Rather than explicitly adding a component to the state layer of our monads, we can abstract this out into its own transformer layer.

Since we intend to use `SupplyT` inside other monads, we'll need to provide a _monad class_.
```Haskell
module Control.Monad.Supply.Class
  ( MonadSupply(..)
  ) where

class Monad m => MonadSupply s m | m -> s where
    supply :: m s
    isExhausted :: m Bool
```
The phrase `| m -> s` is called a "Functional Dependency", and means that `m` _determines_ `s`. This means that any particular monad `m` can only have one `MonadSupply` instance. This is fine for our purposes.

(Beginners with Functional Dependencies should note that this means `StateT Int (State String)` is fundamentally distinct from `State (Int, String)`, because the former doesn't have a `MonadState s` instance where `s` contains `Int`. `s` is forced to `String` by the functional dependency.)

`isExhausted :: MonadSupply s m => m Bool` is provided just in-case. I suspect that every time we need this monad, we will be using an infinite supply, and we won't insert exhausted-checks when our supply is infinite.

Since we also plan on using other monads with `SupplyT`, we need to make sure that if `m` is a `MonadSupply`,  then common transformers on top of `m` are still a `MonadSupply`. These instances are trivial:
```Haskell
instance MonadSupply s m => MonadSupply s (ExceptT e m) where
    supply = lift supply
    isExhausted = lift isExhausted
```
We provide instances for `ExceptT`, `StateT`, `RWST`, `ReaderT`, and `WriterT` (at least, the lazy variants).

Then we'll need an actual implementation:
```Haskell
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Supply where

import Control.Monad.Supply.Class
import Control.Monad.State
import Control.Monad.Identity

newtype SupplyT s m a = SupplyT (StateT [s] m a)
  deriving ( Functor, Applicative, Monad
           , MonadTrans, MonadIO, MonadFix)

runSupplyT :: Monad m => SupplyT s m a -> [s] -> m a
runSupplyT (SupplyT m) init = evalStateT m init
```
Note that we _don't_ derive `MonadState [s]` for `SupplyT`. If we did, and we later put a `SupplyT` on top of a `State`, then attempting to call `supply` would break the functional dependency forced by `State`.

Because we don't derive `MonadState [s]`, we need to manually wrap `get`:
```Haskell
getSupply :: Monad m => SupplyT s m [s]
getSupply = SupplyT get
```

And finally we provide the `MonadSupply` instance:
```Haskell
instance Monad m => MonadSupply m (SupplyT s m) where
    supply = supplyST
    isExhausted = isExhaustedST

supplyST :: Monad m => SupplyT s m s
supplyST = SupplyT $ state $ \s -> (head s, tail s)

isExhaustedST :: Monad m => SupplyT s m Bool
isExhaustedST = SupplyT $ gets null
```

We'll also provide a couple of default supplies for common types. The most important one is a supply for names:
```Haskell
defaultNameSupply :: [String]
defaultNameSupply = [1..] >>= flip replicateM ['a'..'z']
```
This results in the list `["a", "b", ..., "z", "aa", "ab", ...]`.
<br />
The last detail is that the monad class instances go both ways. We've provided instances of `MonadSupply` when a common transformer is on top of a `MonadSupply`, but we could also have a `SupplyT` on top of a common transformer. So we also have to provide instances of the form:
```Haskell
instance MonadError e m => MonadError e (SupplyT s m)
```
These instances can be tricky, and I won't copy them all here. The trick is to unwrap the `SupplyT` action by running it, use the instance from the underlying monad, and then wrap it all back up with 
```Haskell
SupplyT $ StateT $ \s -> ...
```
<h2> Minor Corrections to the Parser </h2>
This section can be skipped, since we'll be upgrading the parser pretty heavily soon.

The main issues with the existing parser are
  - parseModule fails to parse let-expressions
  - let rec expressions aren't recursive
  - let bindings don't allow function sugar

The first problem can be attacked by splitting `exec` in `Interpreter.Main` into two functions, one for loading modules and one for executing toplevel expressions. This would be nice so that loading a module doesn't occasionally evaluate expressions and run them, especially since that doesn't fit the language grammar. We'll take this step later, while upgrading the interpreter. For now, we can provide a quick patch by changing
```Haskell
decl = try letrecdecl <|> letdecl <|> val
```
to
```Haskell
decl = try val <|> try letrecdecl <|> letdecl
```
<br />

To fix the second issue and third issues, we make a simple modification to `letin` (and also to `letrecin`, but as can be seen here I have combined them)

```Haskell
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
```

<h2> Correction to the Type Inferencer </h2>
As noted in some issues on the original Write You a Haskell github page, let-polymorphism is incorrectly implemented. The problem with the original implementation is exemplified by

```Haskell
Poly> \x -> let y = x + 1 in y
<<closure>> :: forall a. Int -> a
```
This type is clearly wrong; it should be `Int -> Int`. What happened was we generated the types `x :: a` and `x + 1 :: b`, and then unify `a ~ Int` and `b ~ Int`. Then we generalize; `y :: forall b. b`. Now in the body of the let we instantiate this scheme to `y :: c`.
Then when constraints were solved, `b ~ Int` never manifests, because no expression has the type `b`.

To fix this, we need to solve the constraints on the rhs _before_ generalizing. Unfortunately, this is slightly out of line with the separation of constraints and solving, but fortunately we can simply re-use our solution to solving the constraints here and keep them separated.

Change
```Haskell
infer expr = case expr of
    ...
    Let x e1 e2 ->
        env <- ask
        t1 <- infer e1
        let sc = generalize env t1
        t2 <- inEnv (x, sc) $ infer e2
        return t2
```
to
```Haskell
infer expr = case expr of
    ...
    Let x e1 e2 ->
        env <- ask
        (t0, cs) <- listen $ infer e1
        subst <- liftEither $ runSolve cs
        let t1 = apply subst t0
            sc = generalize env t1
        t2 <- inEnv (x, sc) $ infer e2
        return t2
```
We use `listen` from `MonadWriter` to get the constraints generated during inference of `e1`, solve those constraints, and apply the solution to `t0`. Then we generalize as before.

<h2> Final Notes </h2>

The `Outputable` class will be used to pretty print compiler output. The compiler will always produce "human-readable" code. The `Outputable` module exports the class and the entire `pretty` library, as well as a few extra helper functions that we'll define as needed.

To avoid orphan instances, I try to put `Outputable` instances for a type at the bottom of the file the type is defined in. In many cases, we'll want to use one type internally to represent, say, errors, and then in a different module we will translate it into another type with a more opaque representation of the information we care about (source code location of the problem, list of contributing factors, etc.) and give _that_ type the `Outputable` instance. This structure is easier to organize and results in well-localized error message generation.
