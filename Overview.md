---


---

<h1> Completing Write You a Haskell </h1>
<p><a href="Table%5C%20Of%5C%20Contents.md">Table of Contents</a></p>
<p>The goal of this project is to try and, in some sense, complete Steven Diehl’s Write You a Haskell. I don’t know if my continuation will have the same level of detail as Diehl’s original work. I plan on having my own project grow linearly as this progresses, but I will attempt to save the work into a separate Github repository at key milestones as I progress. This is a learning experience for me too - code won’t be perfect, and I may make large mistakes. I’m following algorithms presented by papers where possible, and GHC itself otherwise.</p>
<p>I don’t plan on targeting <code>C</code> or <code>llvm</code>. I plan on targetting <code>Java</code>. Partly this is because I’ve written garbage collectors before and don’t feel the need to do it again. <strong>UPDATE</strong>: As of returning to this project, I am now considering targeting <code>C</code> instead, for two reasons. It makes the discussion of the runtime system more interesting, and should make it considerably easier to add an NCG in the future. Considerations will continue. I may start by targeting <code>Java</code> and then have a section later about adding a <code>C</code> backend. A <code>Java</code> backend will (probably) not have an FFI that can understand subclasses. See Eta if you want that.</p>
<p>We’ll be beginning after chapter 7, with the <code>Poly</code> language as a starting point. We’ll be building off of plans laid out in chapter 8, and using some of the parsing ideas presented in chapter 9. The parser will be an Alex lexer + Parsec parser, purely because I’m more comfortable with Parsec then with Happy. Feel free to use Happy instead, as an exercise.</p>
<p>Finally, before beginning, a few words which Diehl omits, presumably since the project never reached a point where they would matter. A compiler, even for a toy language, can become a monolithic piece of software, and there will be lots of code, in lots of files, many of which have similar names (<code>PhExpr</code>, <code>CoreExpr</code>, <code>STGExpr</code>, all containing a type called <code>Expr</code>). Therefore it is <em>critically important</em> that we remain organized and make good use of qualified imports. I think I have a minority opinion that GHC’s use of unqualified module names can make dependencies hard to find and follow for little gain. I’ve rearranged the modules from Chapter 7 as follows:</p>
<pre><code>| - Compiler
  | - Parser
    | - Lexer.hs
    \ - Parser.hs
  | - PhSyn
    | - PhExpr.hs  # The abstract syntax of 
					 ProtoHaskell expressions
    \ - PhType.hs  # The abstract representation of
                     ProtoHaskell types
  | - TypeCheck    # Everything related to type-checking
                     Eventually includes Core
    \ - TcInfer.hs
  \ - Types        # Wide-spread type utilities, like TyEnv
    \ - TyEnv.hs
| - Control 
  \ - Monad        # Custom monadic logic will live here
                     If it is needed in many places
    | - Supply
      | - Class.hs
    \ - Supply.hs # Monad Transformer for monads which store
                    a supply of some type. Thin wrapper
                    around StateT, and will usually be used
                    for names.     
| - Interpreter
  | - Eval.hs
  \ - Main.hs
\ - Utils
  \ - Outputable.hs # Re-exports most of the pretty
					  library and an Outputable class         
</code></pre>
<p>This may seem like overkill now, but the organization will be nice later, even if several folders never grow beyond 2 to 4 modules.</p>
<p>By default, I have the following extensions enabled:</p>
<pre><code>ApplicativeDo
BangPatterns
FlexibleInstances
FunctionalDependencies
GeneralizedNewtypeDeriving
LambdaCase
MultiParamTypeClasses
NamedFieldPuns
OverloadedStrings
PatternGuards
TupleSections
ViewPatterns
</code></pre>
<p>I won’t include <code>LANGUAGE</code> pragmas for these extensions. Note that <code>FlexibleContexts</code> is <em>not</em> on (or implied by) this list. I may occasionally comment on their usage, since I suspect this document will turn out to be attractive towards relatively new Haskellers.</p>
<p>For the same reason as the above, I will try to provide very brief overviews of Haskell idioms the first time they appear, usually a sentence or less.</p>
<p>Finally, it is <em>not</em> my goal that the ProtoHaskell Compiler should be able to compile itself. <code>Control.Monad.Supply.Class</code> already uses <code>UndecidableInstances</code> and I would prefer ease of coding over restricting the subset of GHC-Haskell that I use.</p>

