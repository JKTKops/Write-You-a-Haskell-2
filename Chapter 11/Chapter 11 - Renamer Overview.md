# Renaming

We've done the first two types of program analysis now: lexical analysis and syntax analysis. The next several phases of the compiler perform _semantic analysis_, where the compiler figures out what various pieces of the program _mean_, before it can start translating the program to lower level languages.

The next phase of the compiler is called the _Renamer_. It's primary job is to replace `ParsedName`s with `Name`s - which know where they were defined. We'll call the place where a name is bound its _binding site_. In order to compile a piece of code, we need to know which names refer to which entities. Consider the following example:

```haskell
> import qualified Data.Map as M
> let map = M.fromList [(0, "hi"), (1, "bye")]
    in map
```

Does the second `map` refer to `Interactive.map`, or to `Prelude.map`, which is also in scope? Local bindings _shadow_ higher-level bindings, so it refers to `Interactive.map`. It is the renamer's job to sort this out.

The Renamer has a couple of other purposes too. Remember that the parser parsed all operator applications as left-associative and ignored fixity. The renamer will accumulate the "fixity environment" of the module and then re-associate `OpApp` trees. The renamer also does some treatment of datatype declarations and generates the code for `deriving` clauses.

However, we're going to put a TODO on those parts. Rather than handling the entire, large, `PhSyn` all at once, we'll break it down in parts and manage it piece by piece through the compiler. For now, we'll focus on bindings and the subset of `PhExpr` called the _kernel_. The kernel is closely related to what will be the `Core` language. In fact it's so close, that on the first pass we'll skip the desugarer _entirely_.

TODO: Consider more of PhSyn. We'll come back to this later.

Since we're ignoring the parts of `PhSyn` that allow the user to define data types, the only available types for programs (for now) will be the built-in types and primitives. And since primitive names typically end in `#`, we'll want to hit that TODO at the top of chapter 9 soon, so that we can refer to them.

### Errors and Warnings
The renamer can discover and report some errors and warnings, such as name conflicts. It's important that name conflicts are reported _lazily_; if two names conflict in a scope, but neither of them is used, that's not a problem.

Building the architecture for reporting errors and warnings is an important step of being able to debug the compiler, and we'll start poking at it in this chapter. However, an easy alternative is to use the `panic` function which we will also define in this chapter.

Being able to handle errors "nicely" with error monads is nice. But a compiler can fail pretty much anywhere, with all sorts of errors, and we don't want to have to make every single function in the compiler monadic. So having a few functions that crash immediately with nice messages is helpful and more maintainable.

The same idea will apply to using `unsafePerformIO` in some frequently-used and important places to do impure things when the order of evaluation doesn't matter. For example, we want to cache strings in the compiler so that any two entities with the same (literal) name have a reference to the same `Text` object. This _significantly_ helps the renamer, which can assume that there are no other `OccName`s with the same literal `Text`. In order to create and maintain this cache, we would either need
 1. Every function which can create an `OccName` must be monadic, probably in IO, or
 2. An impure cache hiding in the background via `unsafePerformIO`. 

Since the evaluation order is irrelevant for this cache, as long as it is updated when new names are created, (2) is preferable.

## Chapter Structure
I'm going to try something new with this chapter which I think will help keep it organized. This overview file will contain, well, the overview, and the link to the full source tree. Then I'll include one section for each new core component of the compiler. Some of them are important for this chapter, like the `IOEnv` monad. Some of them aren't, like `panic` and `FastString`. Sometimes a new datatype is just for future-proofing. This chapter includes a new `UniqueMap` type which is just a wrapper over `Data.Map.Map Unique`. However using the `UniqueMap` wrapper instead will make it easier to refactor if you ever decide in the future to use a different/faster scheme for `Unique`s and have to change their shape. When these types come up, I'll comment that they are wrappers, but I won't include a section for them.

There were some new modules in Chapter 9 that I didn't go into much detail for, notably `Utils.Outputable`. Since it's pervasive, I'll give it it's own section along with, but outside of, this chapter. It's a small wrapper over the `pretty` package, but later refactors will make it more complicated, and it's not relevant enough to the chapter content to talk about it every chapter.

## Full Source
todo

move the below into Chapter 10 - IOEnv
## Renamer Monad
Renaming, unsurprisingly, happens in a monad that keeps track of the context we are building. Since the renamer can discover and report errors and warnings, it needs to be in `IO`. Additionally, being in IO lets us use `IORefs` instead of `StateT` layers, which is faster. Therefore, we'll use an `IOEnv` monad:

```haskell
newtype IOEnv env a = IOEnv { unIOEnv :: env -> IO a }
```
Astute readers will recognize this as `ReaderT env IO`. We'll define it manually for entertainment, but most instances could also be obtained by `deriving via (ReaderT env IO)` if you want to use `deriving via`.

Environments will frequently actually be tracking some state using `IORef`s, so we can also export a type synonym, `type MutVar a = IORef a`, and a bunch of functions for working under the `IOEnv` wrapper. Implementations are an easy exercise.

```haskell
newMutVar   :: a -> IOEnv env (MutVar a)
writeMutVar :: MutVar a -> a -> IOEnv env ()
readMutVar  :: MutVar a -> IOEnv env a
updMutVar   :: MutVar a -> (a -> a) -> IOEnv env ()
```


