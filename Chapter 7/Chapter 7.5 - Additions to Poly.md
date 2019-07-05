---


---

<h1> Additions to Poly </h1>
<h2> The SupplyT Monad Transformer </h2>
As explained previously, a monad transformer is a way of adding new capabilities onto an existing monad, where of course monads are like "first-class actions." Frequently throughout compiling, we'll need to grab fresh names (and possibly numbers, etc). Rather than explicitly adding a component to the state layer of our monads, we can abstract this out into its own transformer layer.
<p>Since we intend to use <code>SupplyT</code> inside other monads, we’ll need to provide a <em>monad class</em>.</p>
<pre class=" language-haskell"><code class="prism  language-haskell"><span class="token keyword">module</span> <span class="token constant">Control.Monad.Supply.Class</span>
  <span class="token punctuation">(</span> <span class="token constant">MonadSupply</span><span class="token punctuation">(</span><span class="token operator">..</span><span class="token punctuation">)</span>
  <span class="token punctuation">)</span> <span class="token keyword">where</span>

<span class="token keyword">class</span> <span class="token constant">Monad</span> <span class="token hvariable">m</span> <span class="token operator">=&gt;</span> <span class="token constant">MonadSupply</span> <span class="token hvariable">s</span> <span class="token hvariable">m</span> <span class="token operator">|</span> <span class="token hvariable">m</span> <span class="token operator">-&gt;</span> <span class="token hvariable">s</span> <span class="token keyword">where</span>
    <span class="token hvariable">supply</span> <span class="token operator">::</span> <span class="token hvariable">m</span> <span class="token hvariable">s</span>
    <span class="token hvariable">isExhausted</span> <span class="token operator">::</span> <span class="token hvariable">m</span> <span class="token constant">Bool</span>
</code></pre>
<p>The <code>MonadSupply</code> class is a “multiparam typeclass”, which behaves as the expected generalization of standard Haskell single-param typeclasses. The addition phrase <code>| m -&gt; s</code> is called a “Functional Dependency”, and means that <code>m</code> <em>determines</em> <code>s</code>. This means that any particular monad <code>m</code> can only have one <code>MonadSupply</code> instance. This is fine for our purposes.</p>
<p>(Beginners with Functional Dependencies should note that this means <code>StateT Int (State String)</code> is fundamentally distinct from <code>State (Int, String)</code>, because the former doesn’t have a <code>MonadState s</code> instance where <code>s</code> contains <code>Int</code>. <code>s</code> is forced to <code>String</code> by the functional dependency.)</p>
<p><code>isExhausted :: MonadSupply s m =&gt; m Bool</code> is provided just in-case. I suspect that every time we need this monad, we will be using an infinite supply, and we won’t insert exhausted-checks when our supply is infinite.</p>
<p>Since we also plan on using other monads underneath <code>SupplyT</code>, we need to make sure that <code>SupplyT s m</code> is an instance of a monad class if <code>m</code> is an instance of that monad class. These instances are trivial:</p>
<pre class=" language-haskell"><code class="prism  language-haskell"><span class="token keyword">instance</span> <span class="token constant">MonadSupply</span> <span class="token hvariable">s</span> <span class="token hvariable">m</span> <span class="token operator">=&gt;</span> <span class="token constant">MonadSupply</span> <span class="token hvariable">s</span> <span class="token punctuation">(</span><span class="token constant">ExceptT</span> <span class="token hvariable">e</span> <span class="token hvariable">m</span><span class="token punctuation">)</span> <span class="token keyword">where</span>
    <span class="token hvariable">supply</span> <span class="token operator">=</span> <span class="token hvariable">lift</span> <span class="token hvariable">supply</span>
    <span class="token hvariable">isExhausted</span> <span class="token operator">=</span> <span class="token hvariable">lift</span> <span class="token hvariable">isExhausted</span>
</code></pre>
<p>We provide instances for <code>ExceptT</code>, <code>StateT</code>, <code>RWST</code>, <code>ReaderT</code>, and <code>WriterT</code> (at least, the lazy variants).</p>
<p>Then we’ll need an actual implementation:</p>
<pre class=" language-haskell"><code class="prism  language-haskell"><span class="token comment">{-# LANGUAGE UndecidableInstances #-}</span>
<span class="token keyword">module</span> <span class="token constant">Control.Monad.Supply</span> <span class="token keyword">where</span>

<span class="token import_statement"><span class="token keyword">import</span> Control.Monad.Supply.Class</span>
<span class="token import_statement"><span class="token keyword">import</span> Control.Monad.State</span>
<span class="token import_statement"><span class="token keyword">import</span> Control.Monad.Identity</span>

<span class="token keyword">newtype</span> <span class="token constant">SupplyT</span> <span class="token hvariable">s</span> <span class="token hvariable">m</span> <span class="token hvariable">a</span> <span class="token operator">=</span> <span class="token constant">SupplyT</span> <span class="token punctuation">(</span><span class="token constant">StateT</span> <span class="token punctuation">[</span><span class="token hvariable">s</span><span class="token punctuation">]</span> <span class="token hvariable">m</span> <span class="token hvariable">a</span><span class="token punctuation">)</span>
  <span class="token keyword">deriving</span> <span class="token punctuation">(</span><span class="token constant">Functor</span><span class="token punctuation">,</span> <span class="token constant">Applicative</span><span class="token punctuation">,</span> <span class="token constant">Monad</span><span class="token punctuation">,</span> <span class="token constant">MonadTrans</span><span class="token punctuation">)</span>

<span class="token hvariable">runSupplyT</span> <span class="token operator">::</span> <span class="token constant">Monad</span> <span class="token hvariable">m</span> <span class="token operator">=&gt;</span> <span class="token constant">SupplyT</span> <span class="token hvariable">s</span> <span class="token hvariable">m</span> <span class="token hvariable">a</span> <span class="token operator">-&gt;</span> <span class="token punctuation">[</span><span class="token hvariable">s</span><span class="token punctuation">]</span> <span class="token operator">-&gt;</span> <span class="token hvariable">m</span> <span class="token hvariable">a</span>
<span class="token hvariable">runSupplyT</span> <span class="token punctuation">(</span><span class="token constant">SupplyT</span> <span class="token hvariable">m</span><span class="token punctuation">)</span> <span class="token builtin">init</span> <span class="token operator">=</span> <span class="token hvariable">evalStateT</span> <span class="token hvariable">m</span> <span class="token builtin">init</span>
</code></pre>
<p>Note that we <em>don’t</em> derive <code>MonadState [s]</code> for <code>SupplyT</code>. If we did, and we later put a <code>SupplyT</code> on top of a <code>State</code>, then attempting to call <code>supply</code> would break the functional dependency forced by <code>State</code>.</p>
<p>Because we don’t derive <code>MonadState [s]</code>, we need to manually wrap <code>get</code>:</p>
<pre class=" language-haskell"><code class="prism  language-haskell"><span class="token hvariable">getSupply</span> <span class="token operator">::</span> <span class="token constant">Monad</span> <span class="token hvariable">m</span> <span class="token operator">=&gt;</span> <span class="token constant">SupplyT</span> <span class="token hvariable">s</span> <span class="token hvariable">m</span> <span class="token punctuation">[</span><span class="token hvariable">s</span><span class="token punctuation">]</span>
<span class="token hvariable">getSupply</span> <span class="token operator">=</span> <span class="token constant">SupplyT</span> <span class="token hvariable">get</span>
</code></pre>
<p>And finally we provide the <code>MonadSupply</code> instance:</p>
<pre class=" language-haskell"><code class="prism  language-haskell"><span class="token keyword">instance</span> <span class="token constant">Monad</span> <span class="token hvariable">m</span> <span class="token operator">=&gt;</span> <span class="token constant">MonadSupply</span> <span class="token hvariable">m</span> <span class="token punctuation">(</span><span class="token constant">SupplyT</span> <span class="token hvariable">s</span> <span class="token hvariable">m</span><span class="token punctuation">)</span> <span class="token keyword">where</span>
    <span class="token hvariable">supply</span> <span class="token operator">=</span> <span class="token hvariable">supplyST</span>
    <span class="token hvariable">isExhausted</span> <span class="token operator">=</span> <span class="token hvariable">isExhaustedST</span>

<span class="token hvariable">supplyST</span> <span class="token operator">::</span> <span class="token constant">Monad</span> <span class="token hvariable">m</span> <span class="token operator">=&gt;</span> <span class="token constant">SupplyT</span> <span class="token hvariable">s</span> <span class="token hvariable">m</span> <span class="token hvariable">s</span>
<span class="token hvariable">supplyST</span> <span class="token operator">=</span> <span class="token constant">SupplyT</span> <span class="token operator">$</span> <span class="token hvariable">state</span> <span class="token operator">$</span> <span class="token operator">\</span><span class="token hvariable">s</span> <span class="token operator">-&gt;</span> <span class="token punctuation">(</span><span class="token builtin">head</span> <span class="token hvariable">s</span><span class="token punctuation">,</span> <span class="token builtin">tail</span> <span class="token hvariable">s</span><span class="token punctuation">)</span>

<span class="token hvariable">isExhaustedST</span> <span class="token operator">::</span> <span class="token constant">Monad</span> <span class="token hvariable">m</span> <span class="token operator">=&gt;</span> <span class="token constant">SupplyT</span> <span class="token hvariable">s</span> <span class="token hvariable">m</span> <span class="token constant">Bool</span>
<span class="token hvariable">isExhaustedST</span> <span class="token operator">=</span> <span class="token constant">SupplyT</span> <span class="token operator">$</span> <span class="token hvariable">gets</span> <span class="token builtin">null</span>
</code></pre>
<p>We’ll also provide a couple of default supplies for common types. The most important one is a supply for names:</p>
<pre class=" language-haskell"><code class="prism  language-haskell"><span class="token hvariable">defaultNameSupply</span> <span class="token operator">::</span> <span class="token punctuation">[</span><span class="token constant">String</span><span class="token punctuation">]</span>
<span class="token hvariable">defaultNameSupply</span> <span class="token operator">=</span> <span class="token punctuation">[</span><span class="token number">1</span><span class="token operator">..</span><span class="token punctuation">]</span> <span class="token operator">&gt;&gt;=</span> <span class="token builtin">flip</span> <span class="token hvariable">replicateM</span> <span class="token punctuation">[</span><span class="token char">'a'</span><span class="token operator">..</span><span class="token char">'z'</span><span class="token punctuation">]</span>
</code></pre>
<p>This results in the list <code>["a", "b", ..., "z", "aa", "ab", ...]</code><br>
<br><br>
The last detail is that the monad class instances go both ways. We’ve provided instances of <code>MonadSupply</code> when a <code>MonadSupply</code> is on top of a common transformer, but we could also have a common transformer on top of <code>SupplyT</code>. So we also have to provided instances of the form:</p>
<pre class=" language-haskell"><code class="prism  language-haskell"><span class="token keyword">instance</span> <span class="token constant">MonadError</span> <span class="token hvariable">e</span> <span class="token hvariable">m</span> <span class="token operator">=&gt;</span> <span class="token constant">MonadError</span> <span class="token hvariable">e</span> <span class="token punctuation">(</span><span class="token constant">SupplyT</span> <span class="token hvariable">s</span> <span class="token hvariable">m</span><span class="token punctuation">)</span>
</code></pre>
<p>These instances can be tricky, and I won’t copy them all here. The trick is to unwrap the <code>SupplyT</code> action by running it, use the instance from the underlying monad, and then wrap it all back up with</p>
<pre class=" language-haskell"><code class="prism  language-haskell"><span class="token constant">SupplyT</span> <span class="token operator">$</span> <span class="token constant">StateT</span> <span class="token operator">$</span> <span class="token operator">\</span><span class="token hvariable">s</span> <span class="token operator">-&gt;</span> <span class="token operator">...</span>
</code></pre>
<h2> Minor Corrections to the Parser </h2>
This section can be skipped, since we'll be upgrading the parser pretty heavily soon.
<p>The main issues with the existing parser are</p>
<ul>
<li>parseModule fails to parse let-expressions</li>
<li>let rec expressions aren’t recursive</li>
<li>let bindings don’t allow function sugar</li>
</ul>
<p>The first problem can be attacked by splitting <code>exec</code> in <code>Interpreter.Main</code> into two functions, one for loading modules and one for executing toplevel expressions. This would be nice so that loading a module doesn’t occasionally evaluate expressions and run them, especially since that doesn’t fit the language grammar. We’ll take this step later, while upgrading the interpreter. For now, we can provide a quick patch by changing</p>
<pre class=" language-haskell"><code class="prism  language-haskell"><span class="token hvariable">decl</span> <span class="token operator">=</span> <span class="token hvariable">try</span> <span class="token hvariable">letrecdecl</span> <span class="token operator">&lt;|&gt;</span> <span class="token hvariable">letdecl</span> <span class="token operator">&lt;|&gt;</span> <span class="token hvariable">val</span>
</code></pre>
<p>to</p>
<pre class=" language-haskell"><code class="prism  language-haskell"><span class="token hvariable">decl</span> <span class="token operator">=</span> <span class="token hvariable">try</span> <span class="token hvariable">val</span> <span class="token operator">&lt;|&gt;</span> <span class="token hvariable">try</span> <span class="token hvariable">letrecdecl</span> <span class="token operator">&lt;|&gt;</span> <span class="token hvariable">letdecl</span>
</code></pre>
<br>
To fix the second issue and third issues, we make a simple modification to `letin` (and also to `letrecin`, but as can be seen here I have combined them)
<pre class=" language-haskell"><code class="prism  language-haskell"><span class="token hvariable">letin</span> <span class="token operator">::</span> <span class="token constant">Bool</span> <span class="token operator">-&gt;</span> <span class="token constant">Parser</span> <span class="token constant">Expr</span>
<span class="token hvariable">letin</span> <span class="token hvariable">isrec</span> <span class="token operator">=</span> <span class="token keyword">do</span>
    <span class="token hvariable">reserved</span> <span class="token string">"let"</span>
    <span class="token hvariable">when</span> <span class="token hvariable">isrec</span> <span class="token operator">$</span> <span class="token hvariable">reserved</span> <span class="token string">"rec"</span>
    <span class="token hvariable">x</span> <span class="token operator">&lt;-</span> <span class="token hvariable">identifier</span>
    <span class="token hvariable">args</span> <span class="token operator">&lt;-</span> <span class="token hvariable">many</span> <span class="token hvariable">identifier</span>
    <span class="token hvariable">reservedOp</span> <span class="token string">"="</span>
    <span class="token hvariable">e1</span> <span class="token operator">&lt;-</span> <span class="token hvariable">expr</span>
    <span class="token hvariable">reserved</span> <span class="token string">"in"</span>
    <span class="token hvariable">e2</span> <span class="token operator">&lt;-</span> <span class="token hvariable">expr</span>
    <span class="token keyword">let</span> <span class="token hvariable">rhs</span> <span class="token operator">=</span> <span class="token builtin">foldr</span> <span class="token constant">Lam</span> <span class="token hvariable">e1</span> <span class="token hvariable">args</span>
    <span class="token keyword">if</span> <span class="token hvariable">isrec</span>
      <span class="token keyword">then</span> <span class="token builtin">return</span> <span class="token operator">$</span> <span class="token constant">Let</span> <span class="token hvariable">x</span> <span class="token punctuation">(</span><span class="token constant">Fix</span> <span class="token hvariable">rhs</span><span class="token punctuation">)</span> <span class="token hvariable">e2</span>
      <span class="token keyword">else</span> <span class="token builtin">return</span> <span class="token operator">$</span> <span class="token constant">Let</span> <span class="token hvariable">x</span> <span class="token hvariable">rhs</span> <span class="token hvariable">e2</span>
</code></pre>
<h2> Correction to the Type Inferencer </h2>
As noted in some issues on the original Write You a Haskell github page, let-polymorphism is incorrectly implemented. The problem with the original implementation is exemplified by
<pre class=" language-haskell"><code class="prism  language-haskell"><span class="token constant">Poly</span><span class="token operator">&gt;</span> <span class="token operator">\</span><span class="token hvariable">x</span> <span class="token operator">-&gt;</span> <span class="token keyword">let</span> <span class="token hvariable">y</span> <span class="token operator">=</span> <span class="token hvariable">x</span> <span class="token operator">+</span> <span class="token number">1</span> <span class="token keyword">in</span> <span class="token hvariable">y</span>
<span class="token operator">&lt;&lt;</span><span class="token hvariable">closure</span><span class="token operator">&gt;&gt;</span> <span class="token operator">::</span> <span class="token hvariable">forall</span> <span class="token hvariable">a</span><span class="token punctuation">.</span> <span class="token constant">Int</span> <span class="token operator">-&gt;</span> <span class="token hvariable">a</span>
</code></pre>
<p>This type is clearly wrong; it should be <code>Int -&gt; Int</code>. What happened was we generated the types <code>x :: a</code> and <code>x + 1 :: b</code>, and then unify <code>a ~ Int</code> and <code>b ~ Int</code>. Then we generalize; <code>y :: forall b. b</code>. Now in the body of the let we instantiate this scheme to <code>y :: c</code>.<br>
Then when constraints were solved, <code>b ~ Int</code> never manifests, because no expression has the type <code>b</code>.</p>
<p>To fix this, we need to solve the constraints on the rhs <em>before</em> generalizing. Unfortunately, this is slightly out of line with the separation of constraints and solving, but fortunately we can simply re-use our solution to solving the constraints here and keep them separated.</p>
<p>Change</p>
<pre class=" language-haskell"><code class="prism  language-haskell"><span class="token hvariable">infer</span> <span class="token hvariable">expr</span> <span class="token operator">=</span> <span class="token keyword">case</span> <span class="token hvariable">expr</span> <span class="token keyword">of</span>
    <span class="token operator">...</span>
    <span class="token constant">Let</span> <span class="token hvariable">x</span> <span class="token hvariable">e1</span> <span class="token hvariable">e2</span> <span class="token operator">-&gt;</span>
	    <span class="token hvariable">env</span> <span class="token operator">&lt;-</span> <span class="token hvariable">ask</span>
	    <span class="token hvariable">t1</span> <span class="token operator">&lt;-</span> <span class="token hvariable">infer</span> <span class="token hvariable">e1</span>
	    <span class="token keyword">let</span> <span class="token hvariable">sc</span> <span class="token operator">=</span> <span class="token hvariable">generalize</span> <span class="token hvariable">env</span> <span class="token hvariable">t1</span>
	    <span class="token hvariable">t2</span> <span class="token operator">&lt;-</span> <span class="token hvariable">inEnv</span> <span class="token punctuation">(</span><span class="token hvariable">x</span><span class="token punctuation">,</span> <span class="token hvariable">sc</span><span class="token punctuation">)</span> <span class="token operator">$</span> <span class="token hvariable">infer</span> <span class="token hvariable">e2</span>
	    <span class="token builtin">return</span> <span class="token hvariable">t2</span>
</code></pre>
<p>to</p>
<pre class=" language-haskell"><code class="prism  language-haskell"><span class="token hvariable">infer</span> <span class="token hvariable">expr</span> <span class="token operator">=</span> <span class="token keyword">case</span> <span class="token hvariable">expr</span> <span class="token keyword">of</span>
	<span class="token operator">...</span>
	<span class="token constant">Let</span> <span class="token hvariable">x</span> <span class="token hvariable">e1</span> <span class="token hvariable">e2</span> <span class="token operator">-&gt;</span>
		<span class="token hvariable">env</span> <span class="token operator">&lt;-</span> <span class="token hvariable">ask</span>
		<span class="token punctuation">(</span><span class="token hvariable">t0</span><span class="token punctuation">,</span> <span class="token hvariable">cs</span><span class="token punctuation">)</span> <span class="token operator">&lt;-</span> <span class="token hvariable">listen</span> <span class="token operator">$</span> <span class="token hvariable">infer</span> <span class="token hvariable">e1</span>
		<span class="token hvariable">subst</span> <span class="token operator">&lt;-</span> <span class="token hvariable">liftEither</span> <span class="token operator">$</span> <span class="token hvariable">runSolve</span> <span class="token hvariable">cs</span>
		<span class="token keyword">let</span> <span class="token hvariable">t1</span> <span class="token operator">=</span> <span class="token hvariable">apply</span> <span class="token hvariable">subst</span> <span class="token hvariable">t0</span>
			<span class="token hvariable">sc</span> <span class="token operator">=</span> <span class="token hvariable">generalize</span> <span class="token hvariable">env</span> <span class="token hvariable">t1</span>
		<span class="token hvariable">t2</span> <span class="token operator">&lt;-</span> <span class="token hvariable">inEnv</span> <span class="token punctuation">(</span><span class="token hvariable">x</span><span class="token punctuation">,</span> <span class="token hvariable">sc</span><span class="token punctuation">)</span> <span class="token operator">$</span> <span class="token hvariable">infer</span> <span class="token hvariable">e2</span>
		<span class="token builtin">return</span> <span class="token hvariable">t2</span>
</code></pre>
<p>We use <code>listen</code> from <code>MonadWriter</code> to get the constraints generated during inference of <code>e1</code>, solve those constraints, and apply the solution to <code>t0</code>. Then we generalize as before.</p>
<h2> Final Notes </h2>
<p>The <code>Outputable</code> class will be used to pretty print compiler output. The compiler will always produce “human-readable” code. The <code>Outputable</code> module exports the class and the entire <code>pretty</code> library, as well as a few extra helper functions that we’ll define as needed.</p>
<p>To avoid orphan instances, I try to put <code>Outputable</code> instances for a type at the bottom of the file the type is defined in. In many cases, we’ll want to use one type internally to represent, say, errors, and then in a different module we will translate it into another type with a more opaque representation of the information we care about (source code location of the problem, list of contributing factors, etc.) and give <em>that</em> type the <code>Outputable</code> instance. This structure is easier to organize and results in well-localized error message generation.</p>

