# Lexing


// TODO: context-sensitive lexing for `MagicHash`
// Need start code and `lex` will need to take compiler
// flags.

Just like with our inference system in Poly, there are two systems playing together during parsing. The first system takes the input string and transforms it into a list of meaningful _units_ called `Tokens`. Tokens are things like `TokenLParen` and `TokenId String`. This system is called the _lexer_. The other system is the _parser_, which consumes tokens to construct a _parse tree_. In many cases, the parse tree and the abstract syntax tree of a language are not completely identical. This will be the case for us.

Rather than continuing to do lexing with Parsec, we'll use a real lexer generator called `Alex`. Then we'll use the token stream from `Alex` as the input to a `Parsec` parser. Separating these concerns will simplify using Parsec later.

My main concern with Alex is that it doesn't support `Data.Text`. I could wrap it myself but that seems to be a fairly involved effort. So instead we'll let Alex work on an input `String` and then output `Data.Text.Lazy.Text` everywhere. I suspect that this will become a performance bottleneck - feel free to try supporting `Data.Text` with Alex!

Credit for this lexing strategy (and much of the lexer source) belongs to Simon Marlow, who also maintains Alex  (and GHC!). The lexer is based on the Haskell '98 lexer presented in the `/examples` folder of the Alex source, plus several fixes.

<h3> Alex </h3>

`Alex` is a powerful lexer generator. With Alex, if we can describe the patterns that various tokens fit into, then Alex can split our text up into those tokens. This task is harder than it seems at first glance. Consider this Haskell example.

```Haskell
main = let letresult = 5 in print letresult
```

When we encounter the string `"let"`, we should identify it as a reserved word. But when we encounter `"letresult"`, which has the same prefix, we need to identify it as a variable identifier (`VarId`). There are much more difficult cases to handle correctly; Alex handles this complexity for us.

To use Alex, we'll need to provide a _specification file_, `Lexer.x`, in our source tree. Alex spec files contain a mix of Alex-specific syntax and Haskell source code. Haskell source code is contained inside curly braces. We'll start with a module declaration.

```Haskell
-- Lexer.x
{
module Compiler.Parser.Lexer (Token(..), Lexeme, lex) where

import Prelude hiding (lex)
import Data.Char (isAlphaNum, chr)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

import Utils.Outputable
}
```

Alex will copy this code directly into `Lexer.hs`, and the imports will be available to us in Haskell code blocks later in the source file.

Next we need to tell Alex what mode to run in. Alex has a `basic` mode, which will eventually produce a function `scanTokens :: String -> [Token]`. However it will be more powerful (and easier to extend in the future, if we need) to use the `monad` mode. This makes Alex run inside the `Alex` monad, which is essentially a custom `StateT AlexState (Except String)`. This way if Alex runs into an error, it won't crash the program, which is _very_ important for our interpreter.

```
%wrapper "monad"
```

Now we can start specifying _macros_. Macros can be either _character sets_ or _regular expressions_. Our rules will be represented by regular expressions, and we can expand macros into those. A character set lets us assign a name to a set of characters. Character sets are prefixed with `$`.

```
$whitechar = [\t\n\r\v\f\ ]
$special   = [\(\)\,\;\[\]\{\}]
```

The braces denote a union, so the character set `[A-Z a-z]` is the set of characters that are in the set `A-Z` and the set of the characters in the set `a-z`. The character sets that we're interested in can be copied almost directly out of the Haskell Report, so I won't copy all this boilerplate here.

Next are the _regexes_. A regex is prefixed with `@`. Alex's regular expressions aren't exactly regular expressions, but they're really close. Take a look at the user guide for specifics.
```
@reservedid = case|class|data|default|...

@reservedOp = ".." | ":" | "::" | "=" |...
```
Notice that we reserve the `default` keyword. Even though we might not implement it, it's a good idea to reserve all keywords that we might want to implement in the future. This way we can throw an error rather than accepting a program that is not future-proof.

At the lexer lever, we shouldn't try to separate things by type. Trying to lex an expression with type `Bool` differently than an expression of type `Int` would effectively require implementing a type inference engine in our lexer. The same goes for our parser. But at the lexing level we _can_ separate some things into namespaces, as described in the Haskell Report. Variable names _must_ start with a lowercase letter, module, type, data constructor, and typeclass names _must_ start with an uppercase letter, variable symbols _can't_ start with `:`, and constructor symbols _must_ start with `:`. Our lexer can separate these cases for us, so we'll create distinct patterns for them.

```
@varid  = $small $idchar*
@conid  = $large $idchar*
@varsym = $symbol $symchar*
@consym = \: $symchar*
```

We'll also accept numbers according to the Haskell Report spec, which means we need to support octal, hexadecimal, and exponent notation (for floats).

```
@decimal     = $digit+
@octal       = $octit+
@hexadecimal = $hexit+
@exponent    = [eE] [\-\+] @decimal
```

We also need to provide a regex for `@string`, which is a bit involved. Our `@string` pattern should only match the legal characters of a string, but _not the string itself_. That is, it shouldn't match the start or end quotes. We'll put the quotes into the lexing rule itself.

Now that we have our patterns, we can provide lexing rules. Alex looks for the start of rules by looking for the pattern `:-`. It's common to include the name of the lexing scheme as well, so we put `Haskell :-`.

A rule in Alex starts with a _start code_, enclosed in angle brackets. This lets you tell Alex to only match certain rules in certain circumstances. We're not concerned with this, so we'll always use `0`.

To start, let's tell Alex to ignore whitespace (we'll parse it later using the source positions) and line comments.

```
<0> $white+ { skip }
<0> "--"\-*[^$symchar].* { skip }
```

The Haskell source in braces here is a _continuation_. More on this in a little bit.

ProtoHaskell allows _nested block comments_, which makes it easier to comment out a region of code that itself contains a block comment. This is rather complicated compared to the rest of the lexing logic, so we'll write it for Alex. We need to tell Alex to trigger on `{-` and the rule continuation will handle skipping the comment.

```
"{-" { nested_comment }
```

The _continuations_ in braces are functions that Alex will call once it has a match. `skip` is provided by Alex and simply ignores the match. We can use our own functions as continuations, as seen above; we need to write `nested_comment`. The type of the continuation depends on which wrapper is being used. In our case, we need `AlexInput -> Int -> Alex Token`. Below our rules, we can specify arbitrary Haskell source (in braces), which Alex will copy into the generated lexer. The `lex` function that we exported from the module belongs here, for example.

There's a catch with our tokenizing, though. When we parse, we're going to need position information to support indentation sensitivity. So rather than just creating tokens, we'll create what we'll (slightly inaccurately) refer to as a `Lexeme`, which will store a position as well as the token.

 Let's define some data types that we need and a function to make our `Lexeme`s.

```Haskell
{
-- L position token source
data Lexeme = L AlexPosn TokenType String deriving Eq

data TokenType
     = TokInteger
     | TokFloat
     | TokChar
     | TokString
     | TokSpecial
     | TokReservedId
     | TokReservedOp
     | TokVarId
     | TokQualVarId
     | TokConId
     | TokQualConId
     | TokVarSym
     | TokQualVarSym
     | TokConSym
     | TokQualConSym
     | TokEOF
     deriving (Eq, Show)

-- | Our continuation, for example, @mkL TokVarSym@
mkL :: TokenType -> AlexInput -> Int -> Alex Lexeme
mkL tok (p,_,_,str) len = return $ L p tok (take len str)
}
```

We'll want to export `Lexeme` and `TokenType` too, for our parser.

Now that we have our continuation function, we can start specifying rules that match tokens. With the macros we've defined, this is pretty straightforward.

```
<0> $special                  { mkL TokSpecial }

<0> @reservedid               { mkL TokReservedId }
<0> (@conid \.)+ @varid       { mkL TokQualVarId }
<0> (@conid \.)+ @conid       { mkL TokQualConId }
<0> @varid                    { mkL TokVarId }
<0> @conid                    { mkL TokConId }

<0> @reservedop               { mkL TokReservedOp }
<0> (@conid \.)+ @varsym      { mkL TokQualVarSym }
<0> (@conid \.)+ @consym      { mkL TokQualConSym }
<0> @varsym                   { mkL TokVarSym }
<0> @consym                   { mkL TokConSym }

<0> @decimal
  | 0[oO] @octal
  | 0[xX] @hexadecimal        { mkL TokInteger }


<0> @decimal \. @decimal @exponent?
  | @decimal @exponent        { mkL TokFloat }

<0> \' ($graphic # [\'\\] | " " | @escape) \'
                              { mkL TokChar }

<0> \" @string* \"            { mkL TokString }
```

Handling nested comments is a bit of a mess of special case handling (for example, not breaking on `--}`). Note that in ASCII, `45` is `-`, `123` is `{`, and `125` is `}`.

`alexMonadScan` scans for one lexeme in the `Alex` monad. `alexGetByte` gets the next byte of the given `AlexInput`.

```Haskell
nested_comment :: AlexInput -> Int -> Alex Lexeme
nested_comment _ _ = do
    input <- alexGetInput
    go 1 input
  where go 0 input = alexSetInput input >> alexMonadScan
        go n input = do
            case alexGetByte input of
                Nothing -> err input
                Just (c, input) -> do
                    case chr (fromIntegral c) of
                        '-' -> let temp = input
                               in case alexGetByte input of
                                   Nothing -> err input
                                   Just (125, input) -> go (n-1) input
                                   Just (45,  input) -> go n temp
                                   Just (c, input)   -> go n input
                        '\123' -> case alexGetByte input of
                            Nothing -> err input
                            Just (c, input)
                              | c == fromIntegral (ord '-') -> go (n+1) input
                            Just (c, input) -> go n input
                        c -> go n input
        err input = alexSetInput input >> lexError "error in nested comment"

lexError :: String -> Alex a
lexError s = do
    (p,c,_,input) <- alexGetInput
    alexError $ showPosn p ++ ": " ++ s ++
                      (if not $ null input
                       then " before " ++ show (head input)
                       else " at end of file")

showPosn (AlexPn _ line col) = show line ++ ':' : show col
```

Alex also wants a function to call when it finds the end of input. We'll want a lexeme with a `TokEOF`, and no source. For the position information, anything we put there would be inaccurate, since source contains to characters that could have a position. We will never inspect the position information for this lexeme, so we can safely leave it `undefined`.

```
alexEOF = return $ L undefined TokEOF ""
```

Finally, we provide `lex`.

```Haskell
lex :: String -> Either String [Lexeme]
lex str = runAlex str alexLex

alexLex :: Alex [Lexeme]
alexLex = do lexeme@(L _ tok _) <- alexMonadScan
             if tok == TokEOF
               then return [lexeme]
               else (lexeme:) <$> alexLex
```

Now we have a more powerful lexer that we can use for parsing.

<h3> Telling Alex About Filenames </h3>

This lexer is definitely powerful, but we're about to run into a problem. If we were committed to only compiling single-file programs, we would never need to know filenames. However, eventually, we're going to support multiple-module compilation. This means we're going to need to know the filename being lexed, in order to attach complete location information to it. Alex doesn't know this on its own, so we'll need to provide it. We can do this with a custom _user state_. To start, we need to change our `wrapper` from `%wrapper "monad"` to `%wrapper "monadUserState"`. The Alex-generated code is now identical to before, with two differences. It now has references to two identifiers, `AlexUserState`, and `alexInitUserState`, which we'll need to define.

The only state we care about is the filename, so `type AlexUserState = String` will suffice. We'll initialize this to `""`; `alexInitUserState = ""`.

Recall that the `Alex` monad is effectively a `StateT AlexState (Except String)` monad. To properly initialize our filename, we'll make `lex` take an extra filename parameter and then pass it in so we can read it later.

```Haskell
alexInitFilename :: String -> Alex ()
alexInitFilename fname = Alex $ \s -> Right (s { alex_ust = fname }, ())

-- Utility
alexGetFilename :: Alex String
alexGetFilename = Alex $ \s -> Right (s, alex_ust s)

lex :: String -> String -> Either String [Lexeme]
lex fname input = runAlex input $ alexInitFilename fname >> init <$> alexLex
```

<h3> A Better Token Type </h3>

While we're certainly better off with our `Lexeme`s than we would be with pure tokens, they don't quite have all the information we want. Perhaps most importantly, we only have the _start_ location of each token, but when attaching these locations to the AST, we want to know _end_ locations too.

We can also make an optimization. Once `mkL` knows which _type_ of token we're making, we can sometimes dispatch on the source to get a more specific token. That is, we can change `L pos TokSpecial "("` to the much easier to handle `L pos TokLParen`. To beef up our lexer like this, we'll need some new types and a stronger `mkL` function.

To start, our current `TokenType` constructor names should really contain the word `Type`, to disambiguate from the actual `Token` type that we'll export to go with `Lexeme`. So let's change the `Tok` prefix to `TokType`. Then we'll need our complete type of tokens:

```Haskell
data Token
       -- ^ Special Characters
     = TokLParen
     | TokRParen
     | TokComma
     | TokSemicolon
     | TokLBracket
     | TokRBracket
     | TokLBrace
     | TokRBrace
     | TokBackquote

       -- ^ Literals
     | TokLitInteger Text
     | TokLitFloat   Text
     | TokLitChar    Text
     | TokLitString  Text

       -- ^ Reserved Words
     | TokCase   | TokClass   | TokData   | TokDefault  | TokDeriving
     | TokDo     | TokElse    | TokIf     | TokImport   | TokIn 
     | TokInfix  | TokInfixL  | TokInfixR | TokInstance | TokLet
     | TokModule | TokNewtype | TokOf     | TokThen     | TokType
     | TokWhere

       -- ^ Reserved Operators
     | TokTwoDots -- ".."
     | TokColon | TokDoubleColon | TokEqual  | TokLambda
     | TokBar   | TokLArrow      | TokRArrow | TokAt
     | TokTilde | TokPredArrow

       -- ^ Other
     | TokVarId      Text
     | TokQualVarId  Text Text
     | TokConId      Text
     | TokQualConId  Text Text
     | TokVarSym     Text
     | TokQualVarSym Text Text
     | TokConSym     Text
     | TokQualConSym Text Text
     | TokEOF
     deriving Eq

-- Show source
instance Show Token where
    ...
```

Tokens with `Text` fields store the _literal_ source code which denotes the token. This means `TokLitString` `Text` still has the escape characters and gaps. We'll cheat a bit and use `Parsec` `TokenParser` to parse out our literals from the `Text` fields later, since Parsec's parsers are already designed to parse numbers, chars, and Strings according to Haskell rules.

We also want to store better position information. To this end, we'll add a module, `Compiler/BasicTypes/SrcLoc.hs`.  Here we'll define `SrcLoc` and `SrcSpan`. Each of these is either `Real` or `Unhelpful`. A `Real` location is attached to anything that appears literally in the source. We'll attach `Unhelpful` locations to code generated by the compiler.

```Haskell
data RealSrcLoc = SrcLoc !Text !Int !Int
data SrcLoc = RealSrcLoc !RealSrcLoc
            | UnhelpfulLoc !Text

data RealSrcSpan = SrcSpan { srcSpanFile :: !Text
                           , startLine, startCol, endLine, endCol :: !Int 
                           }
data SrcSpan = RealSrcSpan !RealSrcSpan
             | UnhelpfulSpan !Text
```

We want to separate `Real` and `Unhelpful` locations at the type level, because most of our functions for working with locations will be unsafe with `Unhelpful` locations. We also don't need laziness with these types, so we make them entirely strict to avoid the overhead.

While we're at it, we can define a type `Located e = Located SrcSpan e` for attaching locations to things. Our `Lexeme` type becomes `type Lexeme = Located Token`.

Then we let `mkL` dispatch on the `TokenType` of its first argument.

```Haskell
mkL :: TokenType -> AlexInput -> Int -> Alex Lexeme
mkL toktype (alexStartPos,_,_,str) len = do
    fname <- alexGetFilename
    alexEndPos <- alexGetPos
    let AlexPn _ startLine startCol = alexStartPos
        AlexPn _ endLine endCol = alexEndPos
        startPos = mkSrcLoc fname startLine startCol
        endPos   = mkSrcLoc fname endLine endCol
        srcSpan  = mkSrcSpan startPos endPos
        src = take len str
        cont = case toktype of
            TokTypeInteger    -> mkTok1 TokLitInteger
            TokTypeFloat      -> mkTok1 TokLitFloat
            TokTypeChar       -> mkTok1 TokLitChar
            ...
    return $ cont srcSpan src

mkTok1 :: (String -> Token) -> SrcSpan -> String -> Located Token
```

Actually making the cases is tedious, so it is left as an exercise (see the source). Take care in `TokQualVarSym` - `F..` lexes as `TokQualVarSym "F" "."`, and not as `[TokConId "F", TokTwoDots]`. Similarly, `F.<.>` lexes as `TokQualVarSym "F" "<.>"`.

<h3> Compiling with Alex </h3>

`Cabal` (and by extension, `Stack`) is aware of Alex, since Alex is included in the Haskell Platform. By adding `alex` to the `build-depends` section of your config file, the build system will automatically invoke Alex on your specification file. If you prefer to compile manually, then simply run `$ alex Lexer.x` and Alex will put a `Lexer.hs` file in the same folder.

<h3> Closing Remarks </h3>

I would like to start including a `test` tree in the full sources for these chapters, which will make them slower to produce. I'll try and put out text for a chapter and it's `src` tree first, and correct errors if testing finds them.

