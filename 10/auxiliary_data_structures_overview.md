---
title: Chapter 10 Overview
previous: Parsing
next: Unique and UniqueMap
---
# Auxiliary Data Structures
Like any large project, a compiler needs to move around lots of data. Some of this data is in the form of abstract syntax, which we constructed last chapter. But there is other data too. There is state, which needs to be kept inside of each pass as the compiler works, and there are warnings and errors that need to be handed around somehow.

The modules in this chapter can be done in almost any order, and some of them are optional. Here's what we will do in this chapter:

1. `Unique`s, a way to "uniquely identify" different entities during compilation. The way we will do this is not the most efficient, but it's fairly easy to work with. Some form of `Unique` is required, though it doesn't have to be the same as what is covered here.

2. `UniqueMap`, a mapping from uniques to some other data type. This gives us very fast lookups in maps of entities. If your `Unique` type is just `Int` (or a `newtype` of `Int`), then use `Data.IntMap` as your backing structure instead of what is covered here. Abstracting this is useful in case a later refactor changes the representation of `Unique`s.

3. `Bag`, a set that can contain duplicates. These are also called `MultiSet`s, call yours whatever you like. This is a basic type that isn't really worth writing a section about - you can use GHC's implementation (be sure to include the copyright notice), make a dependency on `Data.MultiSet` from the `multiset` package, or just use lists.

4. `CDoc`, a wrapper on `Doc` from the `prettyprinter` package. This will let us easily construct well-formatted messages, as well as change those messages based on compilation flags. This module is optional, especially if you don't plan on producing good error messages. Pretty printers for our data structures will use `CDoc`, but you can just use `Doc` instead. The `C` is for `Context`. (GHC calls it `SDoc`, but no one in the IRC channel seems to know why)

5. `Messages`. This module contains all of the backing implementation of error and warning messages. If you don't plan on producing good error messages or any warnings, you can pretty much skip this module.

6. `Panic`. This module contains functions for crashing the compiler in various ways, with some nice error messages. We define `panic` is for invariant violations, and `sorry` for unimplemented features. This module is technically optional, but it's very small.

7. `FastString`, a wrapper on `Data.Text.Lazy` that caches each `FastString` as it is created, and returns the existing one instead of creating duplicates. Most importantly, this lets us attach `Unique`s to `FastString`s themselves, since each one contains a unique string. This module is technically optional, but I think it would be more work to skip it.

8. `IOEnv`, a monad which forms the basis of the other monads we will use in the frontend. This module is optional - you could use `ReaderT` over `IO` instead, or `State` and some way of handling failure. Since we're in IO anyway, I'll demonstrate using IO to get statefulness (like `StateT`) and the ability to fail (like `MaybeT`).

If you just want to build a smaller compiler that assumes its input is correct code, you should skip `Messages`, and ignore them in future chapters. Replace throwing of errors with `panic` and don't produce warnings.

`CDoc` is also helpful for suppressing certain types of debugging output. If you skip it, most debugging will probably happen via dumps and ad-hoc traces.
<!--stackedit_data:
eyJoaXN0b3J5IjpbMTc5MDE4NjA0NywxMzMzNDc2ODQ1XX0=
-->