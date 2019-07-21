# Parsing

Unlike the lexer, our parser produces a real AST. That means that it depends on the _infrastructure_ for representing the AST and for representing names in the source. The AST is split across declarations, which can be function declarations (which in turn contain expressions), type signatures, fixity declarations, data type declarations (which can contain type signatures), etc. This turns into a _lot_ of backing infrastructure before we can really start parsing.

## The Infrastructure

Let's dive into the infrastructure that we need to represent all of these types. First off, we have the topmost node of the AST.

```Haskell
data PhModule a = Module
    { modName  :: Maybe (Located Text)
    , modDecls :: [LPhDecl a]
    }
```

A module contains a list of declarations, which are either import statements, type/fixity signatures, data declarations, class declarations, instance declarations, function declarations, or pattern bindings. We'll leave off import statements for now.

The type of declarations is given (for now) by

```Haskell
type LPhDecl a = Located (PhDecl a)
data PhDecl id
     = Binding (PhBind id)
     | Signature (Sig id)
     | DataDecl id [id] [ConDecl id]
     | ClassDecl [Pred id]           -- Superclasses
                 id                  -- Class name
                 id                  -- type variable (The a in 'class Eq a where')
                 (LPhLocalBinds id)  -- Class signatures and default implementations
     | InstDecl [Pred id]            -- Context
                id                   -- Class name
                (PhType id)          -- The type becoming an instance
                (LPhLocalBinds id)   -- class function implementations
```

Constructor declarations are simply a `conid` followed by a list of types.

```Haskell
data ConDecl id
     = ConDecl id [PhType id]
```

We'll extend the language with record syntax later.

A `MatchGroup` is a list of match alternatives along with a context of where the match appears. After parsing, there may be several `MatchGroup`s with one alternative each for any given function name. One of the Renamer's jobs is to unite all the `MatchGroup`s for a single function into one.

```Haskell
data MatchGroup id
     = MG { mgAlts :: [LMatch id]
          , mgCtxt :: MatchContext
          }

type LMatch id = Located (Match id)
data Match id
     = Match { matchPats  :: [LPat id]
             , rhs        :: LRHS id
             , localBinds :: LPhLocalBinds id
             }

data MatchContext
     = FunCtxt
     | CaseCtxt
     | LamCtxt
     | LetCtxt
```

The local bindings in a `Match` are the `where` clause, if any. Each match can have its own `where` clause, which scopes over the entire right hand side, including guards.

```Haskell
type LRHS id = Located (RHS id)
data RHS id
     = Unguarded (LPhExpr id)
     | Guarded [LGuard id]

type LGuard id = Located (Guard id)
data Guard id = Guard (LPhExpr id) (LPhExpr id)
```

If you wanted to implement `PatternGuards`, the type for `Guard` above would need quite a bit of work.

Patterns have a relatively straightforward syntax. Patterns are either a simple variable, a constructor followed by a list of patterns, an "as-pattern" like `xs@(x:_)`, a literal pattern, or a wildcard.

```Haskell
type LPat id = Located (Pat id)
data Pat id
     = PVar id
     | PCon id [Pat id]
     | PAs id (Pat id)
     | PLit PhLit
     | PWild
```

The `Sig` type mentioned above can be either a `TypeSig` or a `FixitySig`. Both of these signatures can simultaneously bind to several entities.

```Haskell
type LSig id = Located (Sig id)
data Sig id
     = TypeSig [id] (LPhType id)
     | FixitySig Assoc Int [id]

data Assoc = Infix | InfixL | InfixR
```

Types are also relatively straightforward. We can have type variables, qualified types (`Eq a => a -> a -> Bool`), and type application. This is enough to describe the entire type system (and in fact is pretty close to the Core type system we'll use later), but we additionally want to extend it with explicit types for the syntax-sugar types that are built-in.

```Haskell
type LPhType id = Located (PhType id)
data PhType id
     = PhVarTy id
     | PhQualTy  [Pred id]    (LPhType id)
     | PhAppTy   (LPhType id) (LPhType id)
     | PhFunTy   (LPhType id) (LPhType id)
     | PhListTy  (LPhType id)
     | PhTupleTy [LPhType id]
```

The type `Pred id` represents an assertion that a type "is in" a typeclass.

```Haskell
data Pred id = IsIn id (PhType id)
```

Finally, that leaves the syntax of expressions themselves. Haskell allows a wide range of expressions, most of which will be desugared into a smaller subset called the _kernel_. The kernel is then easily translated into `Core`.

```Haskell
type LPhExpr id = Located (PhExpr id)
data PhExpr id
     = PhVar id
     | PhLit PhLit
     | PhLam (MatchGroup id)
     | PhApp (LPhExpr id) (LPhExpr id)
     | OpApp (LPhExpr id) -- left operand
             (LPhExpr id) -- operator, should be PhVar
             (LPhExpr id) -- right operand
     | NegApp (LPhExpr id) -- syntactic negation
     | PhCase (LPhExpr id) (MatchGroup id)
     | PhIf (LPhExpr id) (LPhExpr id) (LPhExpr id)
     | PhLet (LPhLocalBinds id) (LPhExpr id)
     | PhDo [LStmt id]
     | ExplicitTuple [LPhTupArg id]
     | ExplicitList [LPhExpr id]
     | ArithSeq (ArithSeqInfo id)
     | Typed (LPhType id) (LPhExpr id)

data PhLit
     = LitInt Int
     | LitFloat Double
     | LitChar Char
     | LitString Text
```

We leave some room for future expansion, to avoid being (completely) overwhelmed. Notably, operator sections are missing, but will be fairly easily included later.

Local bindings can appear in many places. Bindings can be accompanied by `Sig`s, and can bind functions or patterns.

```Haskell
type LPhLocalBinds id = Located (PhLocalBinds id)
type LPhBind id = Located (PhBind id)

data PhLocalBinds id = LocalBinds [LPhBind id] [LSig id]
data PhBind id
     = FunBind id (MatchGroup id)
     | PatBind (LPat id) (LRHS id)
```

Note that pattern bindings can appear at the top level - while using this is rare, the following is a valid Haskell module.

```Haskell
module Example where

Right value = runExcept (return someVal)

x | someBool  = 0
  | otherwise = 1
```

The first pattern binds `value` in the toplevel of the module, while the second binds `x`. If no guard is `True` in a pattern binding, then it is an error when `x` is evaluated. If `x` is never evaluated, then no error should be raised. We can safely translate bindings such as these into a sequence of `if/then/else` expressions during desugaring.

```Haskell
x = if someBool then 0 else if otherwise then 1 
  else error "Non-exhaustive guards in pattern binding for `x'"
```

Inside `do` blocks, we can have three types of statements.

```Haskell
type LStmt id = Located (Stmt id)
data Stmt id
     = SExpr (LPhExpr id)
     | SGenerator (LPat id) (LPhExpr id)
     | SLet (LPhLocalBinds id)
```

This is a toy language, so I'm not sure how involved I want the later work to be. In an ideal world, I'd like to implement the `MonadFail` proposal from the get-go, which would raise a compile-time error if the pattern in a generator statement is not a `PVar` pattern and the `Monad` is not an instance of `MonadFail`. This adds work to the type checker and to the desugarer, as well as adding to the list of entities that need to be wired in.

We've left open the opportunity to support `TupleSections` easily.

```Haskell
type LPhTupArg id = LPhExpr id
```

To support `TupleSections`, one could replace this with a sum type, where one case is a present argument and the other is a missing argument.

Finally we have arithmetic sequences. These are straightforward.

```Haskell
data ArithSeqInfo id
     = From       (LPhExpr id)
     | FromThen   (LPhExpr id)
                  (LPhExpr id)
     | FromTo     (LPhExpr id)
                  (LPhExpr id)
     | FromThenTo (LPhExpr id)
                  (LPhExpr id)
                  (LPhExpr id)
```

Between these types we have the frontend syntax covered. It remains to figure out how to parse it.

# The Parser

