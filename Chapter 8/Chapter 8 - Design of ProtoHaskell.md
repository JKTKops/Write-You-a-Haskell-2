# Design of ProtoHaskell

We're mostly going to follow Diehl's original planning here, but I'm going to reiterate anyway and clean up some details which I may change. This page is a re-write of the original Chapter 8 page from Diehl.

The most relevant detail is that I will refer to the language we are implementing as `ProtoHaskell` through the entire process, including interpreting and generating code. The source provided in this chapter will just be loose files with concrete examples and copies of the code on this page; the files may appear in the source tree later, or they may not.

<h2> Haskell: A Rich Language </h2>

At its core, Haskell is a surprisingly simple and elegant language. But the implementation of a full-powered compiler like GHC is not simple. Even though Haskell can be reduced to a small, expressive subset (called the _kernel_), Haskell itself has had much thought go into making the frontend language so expressive and powerful. Many of the details in doing so are going to require quite a bit of engineering work.

Consider this 'simple' Haskell example, and contrast with what we already have in `Poly`.

```Haskell
filter :: (a -> Bool) -> [a] -> [a]
filter pred [] = []
filter pred (x:xs)
  | pred x    = x : filter pred xs
  | otherwise =     filter pred xs
```

Consider all the things going in this simple example.
  - Lazy evaluation
  - Higher order functions
  - Parametric polymorphism
  - Pattern matching (and defining a function with it)
  - Guards
  - User-provided type annotation
  - List syntactic sugar

Of course Haskell also has custom datatypes, but as we'll see, `Bool` and `[]` have to be somewhat baked in to the compiler.

To handle all these things, we need a much more sophisticated design, and we'll need to track a lot more information about our program during compilation.

<h2> Scope </h2>

This project is intended to be a toy language, so I don't plan on implementing all of Haskell 2010 (but who knows, I might!). However we will definitely implement a sizable portion of Haskell 2010, enough to write real programs and implement all or most of the standard Prelude.

Things we will implement:
  - Indentation sensitive grammar
  - Pattern matching
  - Algebraic data types
  - `where` clauses
  - Recursive functions/types
  - Operator sections
  - Implicit let-rec
  - Syntax sugar for lists, tuples, and arrows
  - Records
  - User-defined operators
  - Do-notation sugar
  - Type annotations
  - Monadic IO
  - Typeclasses (single-param)
  - Arithmetic primops
  - Type synonyms
  - List comprehensions
  - Overloaded literals
  - Optimization passes (and interface files)
  - Newtypes
  - Module namespaces

Things we might implement:
  - Polymorphic recursion
  - GADTSyntax (but not GADTs)
  - Some GHC-specific extensions, like `TupleSections`
  - Foreign Function Interface (to Java)

Things we won't implement
  - GADTs
  - Defaulting rules
  - Exception handling (`error` and `undefined` are guaranteed to crash immediately)

If possible, I'd like ProtoHaskell to conform to the Haskell '98 standard. Either way, it will definitely belong to the "Haskell language family."

<h2> Intermediate Forms </h2>

The basic structure of the compiler will follow fairly closely to that of GHC.
```
Parse -> Rename -> Typecheck -> Desguar
-> Ph2Core -> Simplify -> Core2STG -> STG2Java
```

It's important that we do Typechecking _before_ desugaring, so that in the event of an error message we can easily show the user the code that _they wrote_. We could alternatively do this by passing along lots of annotations about how the code was desugared. However, some desugarings largely complicate or add depth to the AST, so keeping track of these annotations properly would become very complicated. On the other hand, typechecker cases even for `PhExpr` are surprisingly simple (or at least, those for `HsExpr` in GHC are) so writing more cases here will cause less complications.

The `Simplfy` phase contains most of the optimization work. We won't implement too many optimizations, but we'll develop the architecture for "plugging in" rewrites of a Core AST to the simplifier loop. Then the interested reader can write as many optimization passes as they would like.

For the interpreter, we'll intercept the output of the compiler after `Core` and interpret the `Core`. Depending on how this works, we may instead take the `Core` and produce `bytecode` - not Java bytecode, but our own bytecode form which we can interpret.

<h2> Compiler Monad </h2>

The main driver of the compiler will be an `ExceptT` + `StateT` + `IO` stack. All of the compiler passes will hang off this monad.

```Haskell
type CompilerM = ExceptT Msg
                 (StateT CompilerState IO)

data CompilerState = CompilerState
    { fname     :: Maybe FilePath        -- Name of file being compiled
    , imports   :: [FilePath]            -- Filenames of imports
    , src       :: Maybe L.Text          -- Module source
    , ty_env    :: TyEnv.TyEnv           -- Type environment
    , ki_env    :: KiEnv.KiEnv           -- Kind environment
    , cls_env   :: ClsEnv.ClsEnv         -- Typeclass environment
    , c_ast     :: Maybe Core.CoreSyn    -- AST (after core)
    , flags     :: Flags.Flags           -- Compiler flags
    , d_env     :: DataEnv.DataEnv       -- Entity dictionary
    , cls_hrchy :: ClsEnv.ClsHeirarchy   -- Typeclass heirarchy
    }
```
Of course all of this is subject to change, because I'm learning as I go. In particular, the state doesn't contain the `PhSyn` AST. This is because we'll follow GHC's example and parameterize our AST by the type of identifiers in it. We'll manually thread the different AST type through the pipeline. The `CoreSyn` AST on the other hand will only ever have one type of identifier (the `Expr` type will be parameterized by the types of the binders, but it will always be parameterized by `CoreBndr` between compiler phases).

I won't necessarily follow the original chapter plan, but for the next several chapters we will be incrementally building a series of transformations.

```Haskell
parsePhModl  :: FilePath -> L.Text -> CompilerM (PhSyn.PhSyn RdrName)
rename       :: PhSyn.PhSyn RdrName -> CompilerM (PhSyn.PhSyn Name)
typecheckPh  :: PhSyn.PhSyn Name -> CompilerM (PhSyn.PhSyn Id)
desugar      :: PhSyn.PhSyn Id -> CompilerM (PhSyn.PhSyn Id)
ph2Core      :: PhSyn.PhSyn Id -> CompilerM CoreSyn.CoreSyn
simplify     :: CompilerM () -- Now the AST is stored in the compiler state
tidyCore     :: CompilerM ()
prepCore     :: CompilerM ()
core2STG     :: CompilerM StgSyn.StgSyn
stg2Java     :: StgSyn.StgSyn -> CompilerM Java.Syn
```

At the end, we simply pretty-print the Java AST that we've built, along with our runtime system.

For the interpreter, I'm _not worried about performance_. I plan to simply recompile the source of everything in the interpreter environment at each command. When interpreting, we'll intercept the code _before_ the simplifier and execute that. But we'll provide commands to go further into the pipeline and show the results.

After we have all these transformations ready, the compiler itself becomes a straightforward chain of all these transformations.

```Haskell
compileModule :: Flags.Flags -> (IFaceSyn, Java.Syn)
compileModule = runCompilerM $
                parsePhModl
            >=> rename
            >=> typecheckPh
            >=> desugar
            >=> ph2Core
             >> do simplify
                   tidyCore
                   iFace <- core2iFace
                   prepCore
                   core2STG
                   javaSyn <- stg2Java
                   return (iFace, javaSyn)          
```

<h2> Engineering Overview </h2>

Lots of implementation details have already been discussed, but here I'll flesh out the rest. 

Copied straight from the original WYAH:
<h4> REPL </h4>

It is important to have an interactive shell to be able to interactively explore the compilation steps and intermediate forms for arbitrary expressions. GHCi does this very well, and nearly every intermediate form is inspectable. We will endeavor to recreate this experience with our toy language.

If the ProtoHaskell compiler is invoked either in GHCi or as standalone executable, you will see a similar interactive shell.
Command line conventions will follow GHCi's naming conventions. There will be a strong emphasis on building debugging systems on top of our architecture so that when subtle bugs creep up you will have the tools to diagnose the internal state of the type system and detect flaws in the implementation.

Command | Action
--- | ---
:browse | Browse the type signatures for loaded modules
:load <file> | load a program from file
:reload | Reload the active file
:core | Show the core (pre-simpl) of an expression
:modules | Show all loaded module names
:source | Show the source of an expression
:type | Show the type of an expression
:kind | Show the kind of an expression
:set <flag> | Set a flag
:unset <flag> | Unset a flag
:constraints | Dump the typing constraints for an expression
:quit | Exit interpreter

```Haskell
> :type plus
plus :: forall a. Num a => a -> a -> a

> :core id
id :: forall a. a -> a
id = \(ds1 : a) -> a

> :core compose
compose :: forall c d e. (d -> e) -> (c -> d) -> c -> e
compose = \(ds1 : d -> e)
           (ds2 : c -> d)
           (ds3 : c) ->
            (ds1 (ds2 ds3))
```
The flags we use also resemble GHC's and allow dumping out the pretty printed form of each of the intermediate transformation passes. 

-   `-ddump-parsed`
-   `-ddump-rn`
-   `-ddump-desugar`
-   `-ddump-infer`
-   `-ddump-types`
-   `-ddump-core`
-   `-ddump-simpl`
-   `-ddump-stg`
-   `-ddump-java`
-   `-ddump-to-file`

When compiling normally, these flags will just result in the corresponding intermediate form(s) getting dumped to `protohaskellcompiler.dump` (or to a specified file, with `ddump-to-file` on). When interpreting, any flag after `ddump-core` will cause the compiler to go further than it normally would during interactive sessions. It'll go as far down the pipeline as it needs to in order to dump the requested form.

We won't dump to `stdout` since, as mentioned above, the rather not-smart interpreter will recompile everything entered via the current session on every command.

We'll implement the repl with `repline`.

<h4> Parser </h4>

We'll use Alex for lexing and then a normal Parsec parser, with a custom user-state extension for indentation-sensitive parsing.

We _won't_ add operator context sensitivity to the parser. We'll do this the "right way," by parsing all operators with a default precedence and associativity. After parsing, we collect the "true" fixity information and correct the AST. GHC follows this pattern (and this is why errors about orphan fixity declarations don't go with errors about operators not being in scope).

<h4> Renamer </h4>

After parsing, we will traverse the AST and transform each user-named variable from
```Haskell
data RdrName = RdrName L.Text SrcPos
```

to

```Haskell
data Name = Name L.Text Unique SrcPos
type Unique = (Pass, Int)
```

The `Unique`s will be distinct inside each compiler pass. In GHC, a more advanced (and faster) method of generating `Uniq`s is used that generates mere `Int`s, and the same `Uniq` is never generated twice. We won't concern ourselves with this, as it's much trickier to implement and uses the FFI. It's more performant - but I'm not concerned with our compiler performance, rather that of the code the compiler produces.

<h4> Datatypes </h4>

User defined data declarations need to be handled and added to the typing context so that their use throughout the program logic can be typechecked.

```Haskell
data Bool = False | True
data Maybe a = Nothing | Just a
data T1 f a = T1 (f a)
```

Each constructor definition will also introduce several constructor functions into the Core representation of the module. Record types will also be supported and will expand out into selectors for each of the various fields.

<h4> Type Checking </h4>

Type inference and type checking will both happen here. Once the whole program is typechecked, we can transform the identifier type of the AST to its final form.

```Haskell
data Id = Id L.Text Unique Type SrcPos
```

<h4> Desguaring </h4>

There are lots of things to desugar out of the `PhSyn` form, including list forms, and type constructors like `(,)` and `(->)`. But by far the most important is _pattern matching_. The implementation of pattern matching is remarkably subtle, and allows for nested matches and incomplete patterns in the front end language. But these can generate very complex _splitting trees_ of case expressions that need to be expanded. This is one of the things that I don't know how to do yet, and is one of the things I'm most interested in learning via this project.

We will implement the following syntactic sugar translations:
  - `if/then/else` statements into `case` expressions
  - pattern guards into `case` expressions (but probably not the language extension `PatternGuards`)
  - Do-notation for monads
  - list sugar
  - tuple sugar
  - operator sections
  - string literals
  - numeric literals

Unlike Diehl's original plans, I do plan to implement overloaded literals eventually, but not in the "first pass." Overloaded literals means that we replace numeric literals with calls to functions from the `Num` and `Fractional` classes.

```Haskell
-- Frontend
42 :: Num a => a
3.14 :: Fractional a => a

-- Desguared
fromInteger (42 :: Integer)
fromRational (3.14 :: Rational)
```

<h4> Core </h4>

The Core language is the result of translation of the frontend language into an explicitly typed form. Like GHC, we will use a System-F variant, but unlike GHC we will use "vanilla" System-F. GHC has included a couple extensions to System-F in Core which it uses to implement several fancy features. In fact, GHC's core is more accurately called _System-FC_. The interested reader is welcome to implement these extensions on top of the work done here.

The Core language is one of the most defining features of GHC Haskell - the compilation into a statically typed intermediate language. It is a well-engineered detail of GHC's design and it has informed much of how Haskell has evolved. Simon Peyton Jones says "if you can translate it into Core, then [an extension] is really just some form of syntactic sugar. But if it would require an extension to Core, then we have to think a lot more carefully."

```Haskell
data Expr b -- b is the type of binders
     = Var Id
     | Lit Literal
     | App (Expr b) (Arg b)
     | Lam b (Expr b)
     | Let (Bind b) (Expr b)      
     | Case (Expr b) b Type [Alt b]
     | Type Type

-- A general Expr should never be a Type, but an Arg can be
type Arg b = Expr b

-- Case split alternative.
type Alt b = (AltCon, [b], Expr b)
data AltCon
     = DataAlt DataCon
     | LitAlt Literal
     | DEFAULT
     deriving Eq

data Bind b = NonRec b (Expr b)
            | Rec [(b, (Expr b))]

type CoreExpr = Expr CoreBndr
type CoreBndr = Var
```

These definitions are taken directly from GHC - in `compiler/coreSyn/CoreSyn.hs`. However I've removed the features of GHC Core that GHC uses to implement fancy things like coercions. We'll implement newtypes (something that GHC uses coercions for) on a _second_ pass over the project, and we'll do it by effectively replacing newtype wrapping and unwrapping with `id`. Then we'll let it get optimized away. As always, an interested reader is encouraged to implement coercions if they want.

Since Core is covered in explicit types, implementing an internal type checker will be trivial. We'll provide a flag to run this type checker on generated core after desugaring and optimizations. SPJ calls this a "crucial sanity check."

<h4> Type Classes </h4>

We'll implement (only single-parameter) typeclasses with the usual _dictionary passing_ translation. The logic isn't terribly complicated, but can be very verbose and requires lots of bookkeeping about the global typeclass hierarchy. 

Even a simple typeclass can generate some very elaborate definitions.

```Haskell
-- Frontend
class Num a where
    plus :: a -> a -> a

instance Num Int where
    plus = plusInt

plusInt :: Int -> Int -> Int
plusInt (I# a) (I# b) = I# (plusInt# a b)

-- Core
plusInt :: Int -> Int -> Int
plusInt = \(ds1 : Int)
           (ds2 : Int) ->
            case ds1 of {
              I# ds8 ->
                case ds2 of {
                  I# ds9 ->
                    case (plusInt# ds8 ds9) of {
                      __DEFAULT {ds5} -> (I# ds5)
                    }
                }
            }

dplus :: forall a. DNum a -> a -> a -> a
dplus = \(tpl : DNum a) ->
          case tpl of {
            DNum a -> a
          }

plus :: forall a. Num a => a -> a -> a
plus = \($dNum_a : DNum e)
         (ds1 : a)
         (ds2 : a) ->
          (dplus $dNum_a ds1 ds2)
```
We'll subject type classes to the normal restrictions (that is, no `FlexibleInstances`, `FlexibleContexts`, or `UndecidableInstances`)
  - Paterson condition
  - Coverage condition
  - Bounded context stack

<h2> Frontend </h2>

The Frontend language for ProtoHaskell is a fairly large language, consisting of many different types. Let's walk through the different constructions. The frontend syntax is split across several datatypes.

-   `Decls`  - Declarations syntax
-   `Expr`  - Expressions syntax
-   `Lit`  - Literal syntax
-   `Pat`  - Pattern syntax
-   `Types`  - Type syntax
-   `Binds`  - Binders

At the top is the named  _Module_  and all toplevel declarations contained therein. The first revision of the compiler has a very simple module structure, which we will extend later with imports and public interfaces.

```Haskell
data PhSyn = Module Name [Decl] -- ^ module T where { .. }
  deriving (Eq,Show)
```

Declarations or  `Decl`  objects are any construct that can appear at the toplevel of a module. These are namely function, datatype, typeclass, and operator definitions.

```Haskell
data Decl
  = FunDecl BindGroup                    -- ^ f x = x + 1
  | TypeDecl Type                        -- ^ f :: Int -> Int
  | DataDecl Constr [Name] [ConDecl]     -- ^ data T where { ... }
  | ClassDecl [Pred] Name [Name] [Decl]  -- ^ class (P) => T where { ... }
  | InstDecl [Pred] Name Type [Decl]     -- ^ instance (P) => T where { ... }
  | FixityDecl FixitySpec                -- ^ infixl 1  {..}
  deriving (Eq, Show)
```

A binding group is a single line of definition for a function declaration. For instance the following function has two binding groups.

```Haskell
factorial :: Int -> Int

-- Group #1
factorial 0 = 1

-- Group #2
factorial n = n * factorial (n - 1)
```

One of the primary roles of the desugarer is to merge these disjoint binding groups into a single splitting tree of case statements under a single binding group.

```Haskell
data BindGroup = BindGroup
  { matchName  :: Name
  , matchPats  :: [Match]
  , matchType  :: Maybe Type
  , matchWhere :: [[Decl]]
  } deriving (Eq, Show)
```

The expression or  `Expr`  type is the core AST type that we will deal with and transform most frequently. This is effectively a simple untyped lambda calculus with let statements, pattern matching, literals, type annotations, if/then/else statements and do-notation.

```Haskell
data Expr
  = App  Expr Expr        -- ^ a b
  | Var  Name             -- ^ x
  | Lam  Name Expr        -- ^ \\x . y
  | Lit  Literal          -- ^ 1, 'a'
  | Let  Name Expr Expr   -- ^ let x = y in x
  | If   Expr Expr Expr   -- ^ if x then tr else fl
  | Case Expr [Match]     -- ^ case x of { p -> e; ... }
  | Ann  Expr Type        -- ^ ( x : Int )
  | Do  [Stmt]            -- ^ do { ... }
  | Fail                  -- ^ pattern match fail
  deriving (Eq, Show)
```

Inside of case statements will be a distinct pattern matching syntax, this is used both at the toplevel, for function declarations, and inside of case statements.

```Haskell
data Match = Match
  { matchPat :: [Pattern]
  , matchBody :: Expr
  , matchGuard :: [Guard]
  } deriving (Eq, Show)

data Pattern
  = PVar Name              -- ^ x
  | PCon Constr [Pattern]  -- ^ C x y
  | PLit Literal           -- ^ 3
  | PWild                  -- ^ _
  deriving (Eq, Show)
```

The do-notation syntax is written in terms of three constructions, one for monadic binds , one for monadic statements, and one for `let`.

```Haskell
data Stmt
  = Generator Pattern Expr -- ^ pat <- exp
  | Let Pattern Expr       -- ^ let pat = exp
  | Body Expr              -- ^ exp
  deriving (Eq, Show)
```

Literals are the atomic wired-in types that the compiler has knowledge of and will desugar into the appropriate builtin datatypes (and later, to appropriate overloaded function calls).

```Haskell
data Literal
  = LitInt Int           -- ^ 1
  | LitChar Char         -- ^ 'a'
  | LitString [Char]    -- ^ "foo"#
  deriving (Eq, Ord, Show)
```

For data declarations we have two categories of constructor declarations that can appear in the body, regular constructors and record declarations. We will add support for `GADTSyntax` after the first revision.

```Haskell
-- Regular Syntax
data Person = Person String Int

-- GADTSyntax
data Person where
  Person :: String -> Int -> Person

-- Record Syntax
data Person = Person { name :: String, age :: Int }

data ConDecl
  = ConDecl Constr Type                -- ^ T :: a -> T a
  | RecDecl Constr [(Name, Type)] Type -- ^ T :: { label :: a } -> T a
  deriving (Eq, Show, Ord)
```

Fixity declarations are simply a binding between the operator symbol and the fixity information.

```Haskell
data FixitySpec = FixitySpec
  { fixityFix :: Fixity
  , fixityName :: String
  } deriving (Eq, Show)

data Assoc = L | R | N
  deriving (Eq,Ord,Show)

data Fixity = Infix Assoc Int
  deriving (Eq,Ord,Show)
```

Diehl's original Chapter 8 provides many examples for the use of these types, which I will also copy here eventually.

<h2> Traversals </h2>

We'll quite frequently need to run over parts of the AST and replace certain patterns with other patterns. We want to automate this process and abstract it over any of the monads we may be working in.

```Haskell
traverseAstM :: Monad m => (Expr -> m Expr) -> Expr -> m Expr
traverseAstM f e = f e >>= \e' -> case e' of
    App a b   -> App <$> traverseAstM f a <*> traverseAstM f b
    Var a     -> return e'
    Lam a b   -> Lam <$> pure a <*> traverseAstM f b
    Lit n     -> return e'
    Let n a b -> Let <$> pure n <*> traverseAstM f a <*> traverseAstM f b
    If a b c  -> If <$> traverseAstM f a <*> traverseAstM f b <*> traverseAstM f c
    Case a xs -> Case <$> traverseAstM f a <*> traverse (descendCase f) xs
    Ann a t   -> Ann <$> traverseAstM f a <*> pure t
    Fail      -> return e'

descendCase :: Monad m => (Expr -> m Expr) -> Match -> m Match
descendCase f match = case match of
    Match ps a -> Match <$> pure ps <*> traverseAstM f a
```

The case of pure expression rewrites corresponds to the Identity monad.

```Haskell
traverseAst :: (Expr -> Expr) -> Expr -> Expr
traverseAst f e = runIdentity $ traverseAstM (return . f) e
```

This framework will let us do nice things like compose AST rewrites using the fish (`>=>`) operator.

<h2> Closing Remarks </h2>

This is just planning, and we're already at the point where I've likely made a mistake or bad design decision that will cause me headache later. If you know better than me (and if you know much of anything about writing a functional compiler, that's you!) please don't hesitate to file an issue on this repo or shoot me a message on Reddit at u/JKTKops.

