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

Patterns have a relatively straightforward syntax. Patterns are either a simple variable, a constructor followed by a list of patterns, an "as-pattern" like `xs@(x:_)`, a literal pattern, or a wildcard. We'll also extend the constructor case with sugar for tuples and lists.

```Haskell
type LPat id = Located (Pat id)
data Pat id
     = PVar id
     | PCon id [Pat id]
     | PAs id (Pat id)
     | PLit PhLit
     | PWild
     | PTuple [Pat id]
     | PList  [Pat id]
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
     | PhBuiltInTyCon BuiltInTyCon
     | PhQualTy  [Pred id]    (LPhType id)
     | PhAppTy   (LPhType id) (LPhType id)
     | PhFunTy   (LPhType id) (LPhType id)
     | PhListTy  (LPhType id)
     | PhTupleTy [LPhType id]

data BuiltInTyCon
	 = UnitTyCon
	 | ListTyCon
	 | FunTyCon
	 | TupleTyCon Int
```

The `BuiltInTyCon` type is a stand-in until we have an internal representation of these types laid out. Later on, we'll either have the parser produce the internal representation instead of the `BuiltInTyCon` type, or we can have the renamer do a replacement.

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

We leave some room for future expansion, to avoid being (completely) overwhelmed. Notably, operator sections and list comprehensions are missing, but will be fairly easily included later.

Local bindings can appear in many places. Bindings can be accompanied by `Sig`s, and can bind functions or patterns.

```Haskell
type LPhLocalBinds id = Located (PhLocalBinds id)
type LPhBind id = Located (PhBind id)

data PhLocalBinds id = LocalBinds [LPhBind id] [LSig id]
data PhBind id
     = FunBind id (MatchGroup id)
     | PatBind (LPat id) (LRHS id)
```

Note that pattern bindings can appear at the top level - while using this is rare, the following is a valid Haskell module (up to undeclared identifiers).

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

(NB. I may switch to `MegaParsec` when version 8 is released, partly because of the better potential for nice, custom error messages, and partly because of the nicer *default* error messages. Setting up `MegaParsec` to work with custom token streams is much more involved than setting up `Parsec`.)

There are two ways to parse a layout-sensitive grammar. The first way is to translate the token stream from a layout-sensitive form to a layout-*in*sensitive form. For Haskell, this can be done by inserting `{`, `}`, and `;` tokens into the correct places. Unfortunately, by throwing away indentation information in this fashion, we make parse errors drastically worse. GHC takes this approach.

The other way is to make the parser layout-sensitive. This is the approach we will take.

A number of corner cases in the Haskell grammar make the layout-sensitive parsing fairly involved. As a particularly nasty example:

```Haskell
let (x, y) = ("te\
\st", 5) in x
```

This is nasty to parse because it is valid, but the comma on the second line *shouldn't be tested for indentation*. In fact any multi-line string causes the rest of that line to ignore indentation rules. This is very difficult to convey to a parser.

Thankfully, we don't have to leave this issue to the parser to resolve. The Haskell Report recommends introducing a token representing the indentation at the first token of every line, provided that that token is preceded only by whitespace. We can add this capability to our lexer. Since our parser will know the column of the token, it's not important to include the indentation amount.

We could weave this into `Alex` itself, but I find it easier to just add this information in a separate pass.

```Haskell
insertIndentationToks :: [Lexeme] -> [Lexeme]
insertIndentationToks [] = []
insertIndentationToks (l@(Located srcSpan _) : ls) =
    (noLoc TokIndent) : go (l : ls)
  where go [] = []
        go [l] = [l]
        go (l1@(Located s1 _) : l2@(Located s2 _) : ls) =
            -- Test if token l2 is the first on it's line, including the end of token l1
            if (unsafeLocLine $ srcSpanEnd s1) < (unsafeLocLine $ srcSpanStart s2)
            -- If it is, insert indent token
            then l1 : noLoc TokIndent : go (l2 : ls)
            else l1 : go (l2 : ls)

```

Note the indentation token at the start. This will enforce that the indentation of the first token (if it's not the keyword `module`, in which case we'll just ignore the indentation) dictates the indentation of the rest of the file.

This will also make it easier to parse other aspects of indentation sensitivity; we can simply make our primitive `satisfy` parser guard check for indentation tokens, and, if it sees one, guard the indentation of that token against the indentation of the current layout context, if any. So let's set up our parser type. 

```Haskell
type Parser a = Parsec [Lexeme] ParseState a

data ParseState = ParseState
    { compFlags      :: Flags
    , indentOrd      :: Ordering
    , layoutContexts :: [LayoutContext]
    , endOfPrevToken :: SrcLoc
    }

data LayoutContext
     = Explicit
     | Implicit Int

initParseState flags = ParseState flags EQ [] noSrcLoc
```

Parser combinators are *context-free*. This means that a production will act the same no matter where it appears in the grammar. However, the Haskell grammar is *context-sensitive*. That is, some tokens could be interpreted as different parts of the parse tree based on indentation alone, regardless of the rule that accepts them. To handle this, the parser needs to be stateful. By using state to track the context, we can check the context to make correct decisions. Here's the breakdown.

- `compFlags` will be used to make decisions about parsing in the presence of compiler flags. For example, `TupleSections` makes `(a, b,)` legal anywhere that `\c -> (a, b, c)`  is legal.
- `indentOrd` tells the indentation guard what the relative ordering between the reference indentation and the next token needs to be.
- `LayoutContexts` is the major player. By tracking the reference indentations in a stack, whenever we run into a parse error at the end of block, we can simply pop a layout context and try again.
- `endOfPrevToken` will be used to implement the `locate` combinator, which takes a parser and wraps the result in `Located`.

Then we can implement a primitive `satisfy` parser than handles indentation guards, and build everything else on top of that.

```haskell
satisfy :: (Token -> Bool) -> Parser Lexeme
satisfy p = try $ guardIndentation *> satisfyNoIndentGuard <* setIndentOrdGT
  where setIndentOrdGT = modify $ \s -> s { indentOrd = GT }

satisfyNoIndentGuard :: (Token -> Bool) -> Parser Lexeme
satisfyNoIndentGuard p = do
    lexeme@(Located pos _) <- Parsec.tokenPrim
                              prettyShowToken
                              posFromTok
                              testTok
    modify $ \s -> s { endOfPrevToken = mkSrcPos $ srcSpanEnd pos }
    return lexeme

guardIndentation :: Parser ()
guardIndentation = do
    check <- optionMaybe $ satisfyNoIndentGuard (== TokIndent)
    ord <- gets indentOrd
    when (isJust check || ord == EQ) $ do
        mr <- currentLayoutContext
        case mr of
            Nothing -> return ()
            Just Explicit -> return ()
            Just (Implicit r) -> do
                c <- sourceColumn <$> getPosition
                when (c `compare` r /= ord) $ mzero
                    Parsec.<?> "indentation of " ++ show r ++
                               " (got " ++ show c ++ ")"
```

(**Idiom**: the operators `(*>) :: Applicative f => f a -> f b -> f b` and `(<*) :: Applicative f => f a -> f b -> f a` can be used to pick out a particular result from a sequence of applicative (or monadic) actions. We also have `between l r x = l *> x <* r`, but I prefer to use between when `l` and `r` are symmetric, unlike here.)

The `try` around `satisfy` is necessary, because `guardIndentation` might consume a `TokIndent`. In Parsec, token consumption affects behavior. Normally, consuming a token is 1) irreversible and 2) resets "expected" error messages. We may need to check the indentation of a token multiple times if the token could belong to several different implicit layouts. For example;

```haskell
let x = do y <- foo
           return $ bar y
    a = x
in a
```

We'll end up testing the indentation of `a` against the reference for the `do` block, and when that fails we'll need to test it *again* against the indentation of the `let` block. The `try` combinator turns off both effects of token consumption. If the parser fails and consumes input, then we get both backtracking and good error messages! `try` is dangerous for performance when it can cause nested "backtracking trees", but this simple one-shot case won't cause a big complication even if used inside another `try` wrapper.

The check when `ord == EQ` is also necessary, otherwise we fail to reject programs like `let x = 1 y = 2 in x + y`, which should be written as
```haskell
let x = 1
    y = 2 in x + y
```

We'll provide a way to set `indentOrd` to `EQ`:

```haskell
align :: Parser ()
align = modify $ \s -> s { indentOrd = EQ }
```

We can also replace `Parsec`'s `label` combinator with a more useful, layout-sensitive one.

```haskell
label :: Parser a -> String -> Parser a
label p lbl = do
    mctx <- currentLayoutContext
    case mctx of
        Nothing -> Parsec.label p exp
        Just Explicit -> Parsec.label p exp
        Just (Implicit n) -> labelWithIndentInfo p lbl n
  where
    labelWithIndentInfo p lbl n = do
        ord <- gets indentOrd
        let ordPiece = case ord of
                EQ -> show n
                GT -> "greater than " ++ show n
                LT -> "less than" ++ show n
            indentPiece = "at indentation"
        Parsec.label p $ unwords [lbl, indentPiece, ordPiece]

(<?>) = label
```

Then we can start throwing up basic parsers in terms of satisfy.

```haskell
token :: Token -> Parser Lexeme
token t = satisfy (== t) <?> prettyShowToken

oneOf, noneOf :: [Token] -> Parser Lexeme
oneOf ts = satisfy (`elem` ts)

noneOf ts = satisfy (`notElem` ts)

anyToken :: Parser Lexeme
anyToken = satisfy (const True)

reserved :: String -> Parser Lexeme
reserved word = satisfy (== reservedIdToTok word)

reservedOp :: String -> Parser Lexeme
reservedOp op = satisfy (== reservedOpToTok op)

parens, braces, brackets, backticks :: Parser a -> Parser a
parens = between (token TokLParen) (token TokRParen)
...

comma :: Parser ()
comma = void $ token TokComma

commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` comma

semicolon :: Parser ()
semicolon = void $ token TokSemicolon

-- | Separates Haskell expressions by arbitrary numbers of semicolons
stmtSep :: Parser a -> Parser [a]
stmtSep p = many semicolon >> p `sepEndBy` many1 semicolon

-- | Separates at least 1 Haskell expression by arbitrary numbers of semicolons
stmtSep1 :: Parser a -> Parser [a]
stmtSep1 p = many semicolon >> p `sepEndBy1` many1 semicolon
```

With these (and some more) tools in tow, we can define our context-sensitive combinators. Then to parse the grammar we'll just defer to these and otherwise forget about layout!

```haskell
locate :: Parser a -> Parser (Located a)
locate p = do
    startPos <- getPosition
    let srcName = sourceName startPos
        startLine = sourceLine startPos
        startCol  = sourceColumn startPos
        startLoc  = mkSrcLoc (T.pack srcName) startLine startCol
    res <- p
    endPos <- gets endOfPrevToken
    return $ Located (mkSrcSpan startLoc endPos) res

openExplicit :: Parser ()
openExplicit = token TokLBrace >> pushLayoutContext Explicit

closeExplicit :: Parser ()
closeExplicit = token TokRBrace >> popLayoutContext

openImplicit :: Parser ()
openImplicit = do
    c <- sourceColumn <$> getPosition
    pushLayoutContext $ Implicit c

closeImplicit :: Parser ()
closeImplicit = popLayoutContext

block :: Parser a -> Parser [a]
block p = explicitBlock <|> implicitBlock
  where
    explicitBlock = between openExplicit closeExplicit
        $ stmtSep p
    implicitBlock = between openImplicit closeImplicit
        $ concat <$> many (align >> stmtSep1 p) <|> return []

block1 :: Parser a -> Parser [a]
block1 p = explicitBlock1 <|> implicitBlock1
  where
    explicitBlock1 = between openExplicit closeExplicit
        $ stmtSep1 p
    implicitBlock1 = between openImplicit closeImplicit
        $ concat <$> many1 (align >> stmtSep1 p)
```

In the definition of `block`, the last line looks pretty strange. Why write `concat <$> many (align >> stmtSep1 p) <|> return []` instead of merely `concat <$> many (align >> stmtSep p)`? The problem is that `align >> stmtSep p` can succeed without consuming any tokens, because `stmtSep` accepts the empty production. This would cause `many` to hang, but Parsec actually notices that this has happened and instead raises an error. So we have to represent a possibly empty implicit block as "Either an implicit block with something in it, or an empty implicit block." Also notice that, for a similar reason, `block1` needs `many1 (align >> stmtSep1 p)`. If we simply used `many`, then `implicitBlock1` would accept an empty block.

The structure of the parser itself follows pretty directly from the grammar in the [Haskell 2010 report](https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-18000010.5), so rather than go into it in detail here, I suggest reading the source. The key differences are that we won't (yet) support pattern guards or tuple sections.

## Closing

This is probably all I'm going to say about the parser unless you guys want more. There are also some basic tests in the `/test/Compiler/Parser/testcases` directory. I'll be using (mostly) "golden testing" for this project, similar to GHC. The test cases and their expected output is kept in files in the test directory, discovered by the testing engine (see `test/Test.hs`) and run against the expected output. I'm using `tasty-golden` for this. I also have a few small `tasty-hunit` tests. I think as the project progresses, there will continue to be a mix of both. It's staying simple for now, and it will be upgraded as we progress!
